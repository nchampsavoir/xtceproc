-- SPACE SYSTEM MODEL

local class = require '3rdparty.middleclass'
local SLAXML = require '3rdparty.slaxml'
local utils = require 'utils'
local inspect = require "3rdparty.inspect"


-- SpaceSystem is a collection of SpaceSystem(s) including space assets,
-- ground assets, multi-satellite systems and sub-systems.  A SpaceSystem
-- is the root element for the set of data necessary to monitor and
-- command an arbitrary space device - this includes the binary decomposition
-- the data streams going into and out of a device.
local SpaceSystemModel = class('SpaceSystemModel')

local Stack = class('Stack')

function Stack:initialize()
  self._stack = {}
end

function Stack:push(element)
  if element then table.insert(self._stack, element) end
end

function Stack:pop()
  return table.remove(self._stack)
end

function Stack:top()
  return self._stack[#self._stack]
end

function SpaceSystemModel:load_xtce(content)

  local root = {}
  local stack = Stack:new()  
  stack:push(root)

  local lists = {Term=true}
  local sets = {SpaceSystem=true}

  function startElement(name, nsURI, nsPrefix)  
    local element = stack:top()
    local child = {_type = name}
    if lists[name] then 
      child._xlist = string.snakecase(name) 
    elseif sets[name] then 
      child._xset = string.snakecase(name)
    elseif name:sub(-3):lower() == "set" then 
      child._set = true 
    elseif name:sub(-4):lower() == "list" then 
      child._list = true
    end
    stack:push(child)
  end

  function closeElement(name, nsURI, nsPrefix)   
    local child = stack:pop()    
    local parent = stack:top()    
    if parent._set then      
      parent[child.name or #parent] = child
    elseif parent._list then 
      parent[#parent+1] = child
    elseif child._xset then      
      if not parent[child._xset] then parent[child._xset] = {} end     
      parent[child._xset][child.name] = child
    elseif child._xlist then      
      if not parent[child._xlist] then parent[child._xlist] = {} end     
      parent[child._xlist][#parent[child._xlist] + 1] = child
    else 
      parent[string.snakecase(child._type)] = child
    end
  end

  function attribute(name, value, nsURI, nsPrefix) 
    local element = stack:top()
    element[string.snakecase(name)] = value    
  end

  function text(value)    
    local element = stack:top()
    element.text = (element.text or "") .. value
  end

  xtceparser = SLAXML:parser{
    startElement=startElement,
    closeElement=closeElement,
    attribute=attribute,
    text=text
  }

  xtceparser:parse(content, {stripWhitespace=true})

  self.space_system = root.space_system

  -- Post Processing SSM after XTCE load  
  self:attach_all_parameter_types()
  self:attach_all_parameters()
  self:attach_all_containers()

  for _, container in pairs(self.containers) do
    self:process_container(container)    
  end  
end

function SpaceSystemModel:list_space_systems(ssm, indent)
  ssm = ssm or self
  indent = indent or ""  
  if ssm.space_system then
    for name, s in pairs(ssm.space_system) do
      print(indent .. tostring(name))
      self:list_space_systems(s, indent .. "  ")
    end
  end
end

function SpaceSystemModel:get_space_systems(names)
  local ssm = self
  local space_systems = {}
  for i, name in ipairs(names) do
    if not ssm.space_system[name] then
      errorf('Invalid path ' .. path .. ': Space system ' .. name .. ' does not exist.')        
    else
      ssm = ssm.space_system[name]
      table.insert(space_systems, ssm)
    end
  end
  return space_systems
end

function SpaceSystemModel:find(path, predicate)
  local paths = string.split_path(path)  
  local space_systems = self:get_space_systems(paths)
  local res
  for i=#space_systems,1,-1 do
    ssm = space_systems[i]
    res = predicate(ssm)
    if res then break end
  end
  return res
end


-- =================================================
-- PARAMETER TYPES POST PROCESSING
--

function SpaceSystemModel:get_parameter_type(parameter_type_ref, path)  
  local parameter_type
  if parameter_type_ref:sub(1,1) == '/' then
    parameter_type = self.parameter_types[parameter_type_ref]
  else  
    parameter_type = self:find(path, function(ssm) return ssm.telemetry_meta_data.parameter_type_set[parameter_type_ref] end)
  end
  
  assert(parameter_type, "Undefined parameter type " .. parameter_type_ref)
  if not parameter_type._processed then    
    self:process_parameter_type(parameter_type)
  end
  return parameter_type
end

function SpaceSystemModel:attach_all_parameter_types(ssm, path)
  self.parameter_types = self.parameter_types or {}
  ssm = ssm or self
  path = path or ''
  
  if ssm.telemetry_meta_data and ssm.telemetry_meta_data.parameter_type_set then
    for name, parameter_type in pairs(ssm.telemetry_meta_data.parameter_type_set) do
      if name:sub(1,1) ~= '_' then
        self.parameter_types[path .. '/' .. name] = parameter_type
        parameter_type._path = path
      end
    end
  end

  if ssm.space_system then
    for name, sub_system in pairs(ssm.space_system) do      
      self:attach_all_parameter_types(sub_system, path .. '/' .. name)
    end
  end  
end

function align_to_bytes(size_in_bits)
  if size_in_bits == 0 then
    return 0
  elseif size_in_bits <= 8 then
    return 8
  elseif size_in_bits <= 16 then
    return 16
  elseif size_in_bits <= 32 then
    return 32
  elseif size_in_bits <= 64 then
    return 64
  else
    errorf('Unsupported size %d. Maximum is 64 bits.', size_in_bits)
  end
end

function SpaceSystemModel:get_enumeration_table(parameter_type)
  local enums = {}

  if parameter_type._enumeration_table then
    return parameter_type._enumeration_table
  end

  if parameter_type.base_type then
    local base_type = self:get_parameter_type(parameter_type.base_type, parameter_type._path)
    enums = self:get_enumeration_table(base_type)
  end

  if not parameter_type.enumeration_list then
    parameter_type.enumeration_list = {}
  end
  
  for i, enum in ipairs(parameter_type.enumeration_list) do
    if tostring(i):sub(1,1) ~= '_' then
      enums[enum.label] = enum.value
    end
  end    
  
  if table.length(enums) == 0 then
    warnf("Parameter Type %s has no enumerated values", parameter_type.name)  
  end

  return enums
end

function SpaceSystemModel:get_enumerated_value(parameter_type_ref, label)
  local parameter_type = self:get_parameter_type(parameter_type_ref)
  local enums = self:get_enumeration_table(parameter_type)

  if not enums then
    errorf('Parameter type "%s" does not have any enumerated values', 
           parameter_type_ref)
  end
  
  local value = enums[label]
  if not value then
    for k, v in pairs(enums) do
      print(k, "=", v)
    end
    errorf('"%s" does not match any enumerated value for parameter type %s',
           label, parameter_type_ref)
  end

  return value
end

function get_encoding(parameter_type)
  if parameter_type.encoding then return parameter_type.encoding 
  elseif parameter_type.binary_data_encoding then return parameter_type.binary_data_encoding 
  elseif parameter_type.float_data_encoding then return parameter_type.float_data_encoding
  elseif parameter_type.integer_data_encoding then return parameter_type.integer_data_encoding
  elseif parameter_type.string_data_encoding then return parameter_type.string_data_encoding 
  else
    if parameter_type._type == "EnumeratedParameterType" or parameter_type._type == "IntegerParameterType" then
      parameter_type.integer_data_encoding = {
        _type = 'IntegerDataEncoding',
        encoding = "unsigned",
        size_in_bits = "32"
      }
      return parameter_type.integer_data_encoding
    elseif parameter_type._type == "FloatParameterType" then
      parameter_type.float_data_encoding = {
        _type = 'FloatDataEncoding',
        encoding = "IEEE754_1985",
        size_in_bits = "32"
      }
      return parameter_type.float_data_encoding
    elseif parameter_type._type == "BooleanParameterType" then
      parameter_type.integer_data_encoding = {
        _type = 'IntegerDataEncoding',
        encoding = "unsigned",
        size_in_bits = "8"
      }
      return parameter_type.integer_data_encoding
    elseif parameter_type._type == "StringParameterType" then
      parameter_type.string_data_encoding = {
        _type = 'StringDataEncoding',
        encoding = "UTF-8",
        size_in_bits = "0"
      }
      return parameter_type.string_data_encoding
    else
      errorf("Invalid type " .. parameter_type._type .. " for parameter type " .. parameter_type.name)
    end
  end
end

function SpaceSystemModel:process_encoding(encoding)
    -- Integer Encoding    
    local size_in_bits = tonumber(encoding.size_in_bits)
    
    if encoding._type == "IntegerDataEncoding" then        
      if encoding.encoding == "unsigned" then 
        encoding.read_fn = string.format('read_uint%d', size_in_bits)
        encoding.ctype = string.format('uint%d_t', align_to_bytes(size_in_bits))
      elseif encoding.encoding == "twosComplement" then
        encoding.read_fn = string.format('read_int%d', size_in_bits)
        encoding.ctype = string.format('int%d_t', align_to_bytes(size_in_bits))
      elseif encoding.encoding == "IEEE754_1985" then
        if size_in_bits == 32 then
          encoding.ctype = 'float'
          encoding.read_fn = 'read_float'
        elseif size_in_bits == 64 then
          encoding.ctype = 'double'
          encoding.read_fn = 'read_double'
        else
          errorf('Invalid float data length %d for parameter type %s. IEEE754_1985 size must be 32 bits or 64 bits."',
                 size_in_bits, parameter_type.name)
        end
      else
        errorf("Unsupported encoding %s for type %s", encoding.encoding, parameter_type.name)  
      end

    -- Float Encoding
    elseif encoding._type == "FloatDataEncoding" then        
      if size_in_bits == 32 then
        encoding.ctype = 'float'
        encoding.read_fn = 'read_float'
      elseif size_in_bits == 64 then
        encoding.ctype = 'double'
        encoding.read_fn = 'read_double'
      else
        errorf('Invalid float data length %d for parameter type %s. IEEE754_1985 size must be 32 bits or 64 bits."',
               size_in_bits, parameter_type.name)
      end
      
    -- Binary Encoding
    elseif encoding._type == "BinaryDataEncoding" then    
      encoding.ctype = 'binary'
      encoding.read_fn = 'read_binary'
      if type(size_in_bits) == "number" and math.fmod(size_in_bits, 8) ~= 0 then
        errorf('Invalid binary data length %d for parameter type %s. Size in bits must an exact number of bytes."',
               size_in_bits, parameter_type.name)
      end

    -- String Encoding
    elseif encoding._type == "StringDataEncoding" then    
      encoding.ctype = 'binary'
      encoding.read_fn = 'read_binary'
      if type(size_in_bits) == "number" and math.fmod(size_in_bits, 8) ~= 0 then
        errorf('Invalid string length %d for parameter type %s. Size in bits must an exact number of bytes."',
               size_in_bits, parameter_type.name)
      end

    else
      errorf('No encoding for parameter type %s', parameter_type.name)    
    end
end

function SpaceSystemModel:process_parameter_type(parameter_type)

  if parameter_type._processed then return end

  -- Aggregated Parameter Type
  if parameter_type._type == "AbsoluteTimeParameterType" or
     parameter_type._type == "RelativeTimeParameterType" then

    local encoding = get_encoding(parameter_type.encoding)
    self:process_encoding(encoding)

  elseif parameter_type._type ~= "AggregateParameterType" then

    local encoding = get_encoding(parameter_type)
    self:process_encoding(encoding)

  end

  parameter_type._processed = true
end


-- =================================================
-- PARAMETER POST PROCESSING
--

function SpaceSystemModel:get_parameter(parameter_ref)  
  local parameter
  if parameter_ref:sub(1,1) == '/' then
    parameter = self.parameters[parameter_ref]
  else  
    parameter = self:find(path, function(ssm) return ssm.telemetry_meta_data.parameter_set[parameter_ref] end)
  end

  assert(parameter, "Undefined parameter " .. parameter_ref)
  if not parameter._processed then
    self:process_parameter(parameter)
  end
  return parameter
end

function SpaceSystemModel:attach_all_parameters(ssm, path)
  self.parameters = self.parameters or {}
  ssm = ssm or self
  path = path or ''
  
  if ssm.telemetry_meta_data and ssm.telemetry_meta_data.parameter_set then
    for name, parameter in pairs(ssm.telemetry_meta_data.parameter_set) do
      if name:sub(1,1) ~= '_' then
        self.parameters[path .. '/' .. name] = parameter
        parameter._path = path
      end
    end
  end

  if ssm.space_system then
    for name, sub_system in pairs(ssm.space_system) do      
      self:attach_all_parameters(sub_system, path .. '/' .. name)
    end
  end  
end

function SpaceSystemModel:process_parameter(parameter)  
  if parameter._processed then return end

  if not parameter.parameter_type_ref then
    errorf('No type ref for parameter %s', parameter.name)
  end
  local parameter_type = self:get_parameter_type(parameter.parameter_type_ref, parameter._path)

  parameter.type = parameter_type
  parameter._processed = true 
end


-- =================================================
-- SEQUENCE CONTAINER POST PROCESSING
--

function SpaceSystemModel:get_container(container_ref)    
  local container
  if container_ref:sub(1,1) == '/' then
    container = self.containers[container_ref]
  else  
    container = self:find(path, function(ssm) return ssm.telemetry_meta_data.container_set[container_ref] end)
  end

  assert(container, "Undefined sequence container " .. container_ref)
  if not container._processed then
    self:process_container(container)
  end
  return container
end

function SpaceSystemModel:attach_all_containers(ssm, path)
  self.containers = self.containers or {}
  ssm = ssm or self
  path = path or ''
  
  if ssm.telemetry_meta_data and ssm.telemetry_meta_data.container_set then
    for name, container in pairs(ssm.telemetry_meta_data.container_set) do
      if name:sub(1,1) ~= '_' then
        self.containers[path .. '/' .. name] = container
        container._path = path
      end
    end
  end

  if ssm.space_system then
    for name, sub_system in pairs(ssm.space_system) do      
      self:attach_all_containers(sub_system, path .. '/' .. name)
    end
  end  
end

function SpaceSystemModel:get_entry(container, parameter_ref)
  for i, entry in pairs(container.entry_list) do
    if tostring(i):sub(1,1) ~= '_' then
      if parameter_ref == entry.parameter_ref then return entry end
    end
  end  
end

function SpaceSystemModel:get_read_ahead_entry(container, parameter_ref)
  for i, entry in pairs(container.read_ahead_entry_list) do
    if parameter_ref == entry.parameter_ref then return entry end
  end
end

function SpaceSystemModel:add_child_container(parent, child)
  local restriction_criteria = child.base_container.restriction_criteria
  assert(restriction_criteria, "Container " .. child.name .. " has no restriction criteria")
  local comparison_list = restriction_criteria.comparison_list
  assert(restriction_criteria, "Container " .. child.name .. " has no comparison list")

  if not parent.children then
    parent.children = {}
  end

  parent.children[child.name] = child
  -- print(inspect(comparison_list))
  
  for k, comparison in pairs(comparison_list) do 
    if tostring(k):sub(1,1) ~= '_' then      
    
      -- If the comparison is defined on a parameter belonging to the child container
      -- (YES it's ugly, and YES xtce allows that) we need to move the parameter entry
      -- on which the comparison is defined and all the preceeding entries to the parent
      -- container, so that the parent container can do the branching.
      local parameter_entry = self:get_entry(child, comparison.parameter_ref)
      if parameter_entry then  
        parent.read_ahead_entry_list = parent.read_ahead_entry_list or {}      
        
        -- If the parameter entry has not already been added to the parent read aheads
        if not self:get_read_ahead_entry(parent, comparison.parameter_ref) then
                  
          for i, entry in pairs(child.entry_list) do
            local entry = parent.read_ahead_entry_list[i]
          
            if not entry then
              -- Copy the entry to the parent if this has not already been done by another child container
              parent.read_ahead_entry_list[i] = entry
              break
          
            elseif entry.parameter_ref ~= entry.parameter_ref then 
              -- If the parameter has already been contributed to the parent, check that the
              -- parent read ahead entries are similar to the ones the container would contribute
              -- else fail loudly
              errorf('Inclusion condition on parameter %s defined in container %s is coming after ' ..
                     'parameter entry %s which is not coherent with the other children of ' ..
                     'container %s.', comparison.parameter_ref, child.name, entry.parameter_ref, parent.name)
            end
          end 
        end      
      end   
    end
  end

  if not parent.branches then
    parent.branches = {}
  end
  
  parent.branches[toidentifier(child._path .. "/" .. child.name)] = comparison_list
end


function SpaceSystemModel:process_container(container)
  if container._processed then return end
  
  container.tail_entry_list = {}  
  for entry_number, entry in pairs(container.entry_list) do
    if tostring(entry_number):sub(1,1) ~= '_' then
    
      -- move located container to tail  
      local location = entry.location_in_container_in_bits
      if location ~= nil then
        if location.reference_location == "containerEnd" then
          container.tail_entry_list[#container.tail_entry_list+1] = entry
          container.entry_list[entry_number] = nil
        else
          errorf('Invalid location reference %s for entry #%d of container %s',
                 location.reference_location, entry_number, container.name)
        end
      end

    end
  end

  -- add this container as a child of the base container
  if container.base_container then    
    base = self:get_container(container.base_container.container_ref)
    self:add_child_container(base, container)
  end  
  
  container._processed = true
end


-- =================================================
-- Lua Code Generation
--

local Stream = class('Stream')

function Stream:initialize(file_object, tab_size)
  self.f = file_object
  self._indent_level = 0
  self._tab_size = tab_size or 2
end

function Stream:indent()
  self.f:write(string.rep(" ", self._indent_level * self._tab_size))
end

function Stream:write(fmt, ...)
  self.f:write(string.format(fmt, ...))
end

function Stream:writeln(fmt, ...)
  self:indent()
  self:write(fmt .. '\n', ...)
end

function Stream:indent_more()
  self._indent_level = self._indent_level + 1
end

function Stream:indent_less()
  self._indent_level = self._indent_level - 1
end

function toidentifier(path)
  return string.join('_', string.split_path(path))
end

function SpaceSystemModel:write_enumerations(f)
  for name, parameter_type in pairs(self.parameter_types) do
    if parameter_type._type == "EnumeratedParameterType" then
      assert(parameter_type, "Undefined parameter type " .. parameter_type.name)
      local enums = self:get_enumeration_table(parameter_type)

      if enums then
        local  parameter_type_ref = parameter_type._path .. '/' .. parameter_type.name
        f:writeln("-- Value choices for %s", parameter_type.name)
        f:writeln("%s_CHOICES = {", toidentifier(parameter_type_ref))
        for label, value in pairs(enums) do
          f:writeln('  [%s]="%s",', value, label)
        end
        f:writeln("}")
        f:writeln("")
      end
    end

  end  
end

function SpaceSystemModel:write_calibrators(f)
  f:writeln("function calibrate(value, raw_values, calibrated_values, extrapolate)")
  f:writeln("  local i=1")
  f:writeln("  local r")
  f:writeln("  for i, r in ipairs(raw_values) do")
  f:writeln("    if value >= r then break end")
  f:writeln("  end")
  f:writeln("  if extrapolate then")
  f:writeln("    local c2 = calibrated_values[i+1]")
  f:writeln("    local c = calibrated_values[i]")
  f:writeln("    local r2 = raw_values[i+1]")
  f:writeln("    return c + (value - r) * ((c2 - c) / (r2 - r))")
  f:writeln("  else")
  f:writeln("    return calibrated_values[i]")
  f:writeln("  end")
  f:writeln("end")
  f:writeln("")

  for name, parameter_type in pairs(self.parameter_types) do

    local spline_calibrator = parameter_type.default_calibrator and parameter_type.default_calibrator.spline_calibrator
    if spline_calibrator then
      self:write_spline_calibrator(f, parameter_type, spline_calibrator)
    end

  end  
end


function SpaceSystemModel:write_root_fn(f, root)
  f:writeln('-- This function represents the root of the decommutation tree and must')
  f:writeln('-- be called on every packet. It reads its data at *start_of_packet* position in')
  f:writeln('-- a buffer object and store its results in a preallocated *values* struct')
  f:writeln('-- in the provided context')
  f:writeln('function root(context, start_of_packet_in_bits)')
  f:writeln('  return coroutine.wrap(function()')
  f:writeln('    local end_of_packet_in_bits = %s(context, start_of_packet_in_bits, expected_packet_length)', toidentifier(root))
  if not self.production then
    f:writeln('    if context.debug then')
    f:writeln('      local expected_packet_length = (context.header_length + context.data_field_length) * 8')  
    f:writeln('      local size_in_bits = end_of_packet_in_bits - start_of_packet_in_bits')  
    f:writeln('      if expected_packet_length ~= nil and size_in_bits ~= expected_packet_length then')
    f:writeln('        error(string.format("Incorrect packet length: got %%d bits, expected %%d bits", size_in_bits, expected_packet_length))')
    f:writeln('      end')
    f:writeln('    end')
  end
  f:writeln('  end)')
  f:writeln('end')
  f:writeln('')
end

function expr_from_scale_and_offset(scale, offset)
  local expr = ""
  if scale ~= 1 then
    expr = expr .. string.format(" * %s", scale)
  end
  if offset ~= 0 then
    expr = expr .. string.format(" + %s", offset)
  end
  return expr
end

function polynomial_expr(varname, constant, coefficients)
  local expr = ""
  for exponent, coefficient in ipairs(coefficients) do
    if coefficient ~= 0 then
      
      if coefficient ~= 1 then
        expr = expr .. " " .. coefficient .. "*"
      end

      if exponent == 1 then
        expr = expr .. " " .. varname
      else
        expr = expr .. string.format(" math.pow(%s, %s)", varname, exponent)
      end
    end
  end
  if constant ~= 0 then
    expr = expr .. string.format(" + %s", constant)
  end
  return expr
end

function SpaceSystemModel:write_polynomial_calibrator_expression(f, parameter_type, calibrator)
  -- A calibration type where a curve in a raw vs calibrated plane is described using a set of
  -- polynomial coefficients.  Raw values are converted to calibrated values by finding 
  -- a position on the curve corresponding to the raw value. The first coefficient belongs with
  -- the X^0 term, the next coefficient belongs to the X^1 term and so on. 
  local coefficients = {}
  local constant = 0
  for _, term in pairs(calibrator.term) do  
    if tonumber(term.exponent) == 0 then
      constant = tonumber(term.coefficient)
    else
      coefficients[tonumber(term.exponent)] = tonumber(term.coefficient)
    end
  end
  f:writeln("cal_val = %s", polynomial_expr("raw_val", constant, coefficients))
end


function SpaceSystemModel:write_spline_calibrator(f, parameter_type, calibrator)
  -- A calibration type where a segmented line in a raw vs calibrated plane is described using
  -- a set of points.  Raw values are converted to calibrated values by finding a position
  -- on the line corresponding to the raw value. The algorithm triggers on the input parameter.
  local parameter_type_ref = parameter_type._path .. '/' .. parameter_type.name
  local raw_values = {}
  local calibrated_values = {}
  local i = 1  
  local extrapolate = calibrator.extrapolate and calibrator.extrapolate:lower() == "true"
  extrapolate = extrapolate and "true" or "false"
  
  for _, spline_point in pairs(calibrator.spline_point) do  
    i = spline_point.order and tonumber(spline_point.order) or i
    raw_values[i] = spline_point.raw
    calibrated_values[i] = spline_point.calibrated
  end
  f:writeln("-- Spline Calibrator")
  f:writeln("function %s_calibrator(value)", toidentifier(parameter_type_ref))
  f:indent_more()
  f:writeln("local raw_values = {%s}", string.join(", ", raw_values))
  f:writeln("local calibrated_values = {%s}", string.join(", ", calibrated_values))
  f:writeln("return calibrate(value, raw_values, calibrated_values, %s)", extrapolate)  
  f:indent_less()
  f:writeln("end")
  f:writeln("")  
end


function SpaceSystemModel:write_calibrator_expression(f, parameter_type, calibrator)
  if calibrator.polynomial_calibrator then
    self:write_polynomial_calibrator_expression(f, parameter_type, calibrator.polynomial_calibrator)
  elseif calibrator.spline_calibrator then
    local parameter_type_ref = parameter_type._path .. '/' .. parameter_type.name
    f:writeln("cal_val = %s_calibrator(raw_val)", toidentifier(parameter_type_ref))
  else
    errorf('Unsupported calibrator type for parameter type "%s".', parameter_type.name)
  end
end

function SpaceSystemModel:write_parameter_entry(f, container, entry, parameter, comment, tail, read_ahead)  

  local parameter_type = self:get_parameter_type(parameter.parameter_type_ref, parameter._path)  
  local parameter_ref = parameter._path .. '/' .. parameter.name
  local varname = toidentifier(parameter_ref)
  if not self.production and #comment > 0 then f:writeln(comment) end

  -- aggregates
  if parameter_type._type == "AggregateParameterType" then

    for k, member in pairs(parameter_type.member_list) do
      if tostring(k):sub(1,1) ~= '_' then
        local member_type = self:get_parameter_type(member.type_ref, parameter_type._path)
        local member_as_parameter = {
          name = varname .. "__" .. member.name,
          parameter_type_ref = member.type_ref,
          _path = parameter._path
        }
        self:write_parameter_entry(f, container, entry, member_as_parameter, member_type.short_description or "", tail, read_ahead)  
      end
    end  

  else

    local encoding
    local scaling_expr = ""    
    local location_var = read_ahead and "read_ahead_location_in_bits" or "location_in_bits"

    if parameter_type._type == "AbsoluteTimeParameterType" or 
       parameter_type._type == "RelativeTimeParameterType" then
      encoding = get_encoding(parameter_type.encoding)
      -- FIXME: Fix possible numerical precision loss 
      local scale = tonumber(parameter_type.encoding.scale)      
      local offset = tonumber(parameter_type.encoding.offset)
      scaling_expr = expr_from_scale_and_offset(scale, offset)
    else
      encoding = get_encoding(parameter_type)
    end

    if encoding._type ~= "BinaryDataEncoding" and
       encoding._type ~= "StringDataEncoding" and
       encoding._type ~= "IntegerDataEncoding" and
       encoding._type ~= "FloatDataEncoding" then
      if tail then
        errorf('Invalid type %s for tail entry %s for container %s', encoding._type, parameter_ref, container_name)
      else
        errorf('Invalid type %s for entry %s for container %s', encoding._type, parameter_ref, container_name)
      end
    end

    local size_in_bits = encoding.size_in_bits
    local variable_length = encoding._type == "BinaryDataEncoding" or encoding._type == "StringDataEncoding"

    -- read raw value
    if variable_length then 
      if type(size_in_bits) ~= "number" then
        if size_in_bits.dynamic_value then
          size_in_bits = self:get_dynamic_value_fragment(size_in_bits.dynamic_value)
        else          
          size_in_bits = tonumber(size_in_bits.fixed_value.text)
        end
      end
      f:writeln('raw_val = buffer:%s(%s, %s)%s', encoding.read_fn, location_var, size_in_bits, scaling_expr)  
    else
      f:writeln('raw_val = buffer:%s(%s)%s', encoding.read_fn, location_var, scaling_expr)
    end  

    -- move cursor forward
    f:writeln('%s = %s + %s', location_var, location_var, size_in_bits)      

    -- compute calibrated value value    
    if encoding.default_calibrator then
      self:write_calibrator_expression(f, parameter_type, encoding.default_calibrator)
    elseif encoding.context_calibrator_list then
      -- FIXME: Add support for context calibrators
      f:writeln('cal_val = raw_val')     
    else    
      f:writeln('cal_val = raw_val')     
    end

    -- add value to white board 
    f:writeln('values.%s = cal_val', varname)

    -- add value to history
    f:writeln('history.%s:push(cal_val)', varname)  

    -- print computed values if debug mode is on
    if not self.production then
      f:writeln("if context.debug then")
      f:writeln("  print(string.format('    %s = %%s (raw value: %%s, length: %%d)', cal_val, raw_val, %s))", parameter_ref, size_in_bits)
      f:writeln("end")
    end

    -- if value is an enumerated, convert numerical representation to the actual string value
    if parameter_type._type == "EnumeratedParameterType" then  
      local parameter_type_ref = parameter_type._path .. '/' .. parameter_type.name
      f:writeln('cal_val = %s_CHOICES[cal_val]', toidentifier(parameter_type_ref))
    end

    -- yield the computed values
    f:writeln('coroutine.yield("%s", raw_val, cal_val, monitoring, status)', parameter_ref)
    f:writeln("")
      
  end
end

function SpaceSystemModel:write_container_entry(f, container, entry, tail)  
  f:writeln('location_in_bits = %s(context, location_in_bits)', toidentifier(entry.container_ref))  
  f:writeln("")
end

function SpaceSystemModel:get_dynamic_value_fragment(dynamic_value)
  local parameter_ref = dynamic_value.parameter_instance_ref.parameter_ref
  local varname = toidentifier(parameter_ref)

  if dynamic_value.linear_adjustment ~= nil then      
    local slope = tonumber(dynamic_value.linear_adjustment.slope)
    local intercept = tonumber(dynamic_value.linear_adjustment.intercept)
    if not slope and not intercept then
      errorf("Linear adjustment for parameter %s must have either a slope value or an intercept value.",
             parameter_ref, container_name)
    elseif slope and slope ~= 1 and intercept and intercept ~= 0 then
      local sign = (intercept > 0) and "+" or "" 
      return string.format('values.%s*%d%s%d', varname, slope, sign, intercept)
    elseif slope and slope ~= 1 then
      return string.format('values.%s*%d', varname, slope)
    elseif intercept and intercept ~= 0 then
      local sign = (tonumber(intercept) >= 0) and "+" or "" 
      return string.format('values.%s%s%d', varname, sign, intercept)
    end
  end

  return string.format('values.%s', parameter_ref)
end

function SpaceSystemModel:write_entry(f, container, entry_number, entry, tail, read_ahead)  

  if not entry.parameter_ref and not entry.container_ref then
    if tail then
      errorf("Entry must have parameter_ref or containter_ref for tail entry #%d for container %s", entry_number, container.name)
    else
      errorf("Entry must have parameter_ref or containter_ref for entry #%d for container %s", entry_number, container.name)
    end
  end

  if entry.include_condition then    
    local comparison = entry.include_condition.comparison
    local parameter = self:get_parameter(comparison.parameter_ref, container._path)
    local parameter_type = self:get_parameter_type(parameter.parameter_type_ref, container._path)
    local encoding = get_encoding(parameter_type)
    if not encoding then
      errorf("Invalid parameter type encoding for Include Condition in container %s", container.name)
    end

    f:writeln("-- evaluate the include condition on this entry ")        
    local varname = toidentifier(comparison.parameter_ref)
    comparison.operator = comparison.operator or "=="    
    if parameter_type._type == "EnumeratedParameterType" then      
      f:writeln("if values.%s %s %s then", varname, comparison.operator, self:get_enumerated_value(parameter.parameter_type_ref, comparison.value))    
    elseif encoding._type == "IntegerDataEncoding" or encoding._type == "FloatDataEncoding" then
      f:writeln("if values.%s %s %s then", varname, comparison.operator, comparison.value)        
    elseif encoding._type == "BinaryDataEncoding" or encoding._type == "StringDataEncoding" then
      f:writeln('if values.%s %s "%s" then', varname, comparison.operator, comparison.value)    
    else
      errorf('Unsupported encoding type "%s" for Include Condition in container %s', encoding._type, container.name)
    end
    f:indent_more()
  end  

  if entry.repeat_entry then    
    local count = entry.repeat_entry.count
    if count.dynamic_value then    
      f:writeln("-- this entry is repeated a number of times that depends")   
      f:writeln("-- on a previous parameter value inside this packet")   
      count = self:get_dynamic_value_fragment(count.dynamic_value)
    else
      f:writeln("-- this entry is repeated %d times", count)   
    end
    if not self.production then
      f:writeln("if context.debug then")
      f:writeln("  print(string.format('Loop: %%d times', %s))", count) 
      f:writeln("end") 
    end
    f:writeln('for i=0,%s do', count)
    f:indent_more()
    if not self.production then
      f:writeln("if context.debug then")
      f:writeln("  print(string.format('Loop Interation: i=%%d', i))")  
      f:writeln("end")
    end
    f:writeln("")
  end
  
  -- Parameter
  if entry.parameter_ref then
    -- add a comment if any
    local comment = ""      
    if entry.short_description then
      comment = string.format("-- %s %s", entry.parameter_ref, entry.short_description)
    end

    parameter = self:get_parameter(entry.parameter_ref, container._path)
    self:write_parameter_entry(f, container, entry, parameter, comment, tail, read_ahead)    
    
  -- Sub container
  elseif entry.container_ref then
    if read_ahead then
      errorf('Container Entry %s of container %s can not be used as a read_ahead entry', entry.container_ref, container.name)
    end
    self:write_container_entry(f, container, entry, tail)
  end

  if entry.repeat_entry then
    f:writeln("")
    f:indent_less()
    f:writeln("end")
  end


  if entry.include_condition then
    f:indent_less()
    f:writeln('end')  
  end
end

function SpaceSystemModel:has_parameter_entries(container)
  for entry_number, entry in pairs(container.entry_list) do
    if tostring(entry_number):sub(1,1) ~= '_' then
      if entry.parameter_ref then return true end
    end
  end 
  if container.read_ahead_entry_list and #container.read_ahead_entry_list > 0 then   
    return true
  end
  for entry_number, entry in pairs(container.tail_entry_list) do
    if entry.parameter_ref then return true end
  end
  return false
end

function SpaceSystemModel:write_container(f, name, container)
  if container.long_description then
    f:writeln("-- %s", container.long_description.text)  
  end  
  f:writeln("function %s(context, location_in_bits)", toidentifier(name))
  f:writeln("")
  f:indent_more()

  if not self.production then
    f:writeln("if context.debug then print('SequenceContainer: %s') end", name)  
  end
  
  f:writeln("local values = context.values")
  if self:has_parameter_entries(container) then
    f:writeln("local buffer = context.buffer")
    f:writeln("local history = context.history")
    f:writeln("local raw_val, cal_val, validity, status")    
  end
  f:writeln("")

  -- sequence container entries
  if #container.entry_list > 0 then
    f:writeln("-- process every entries in the container")    
    f:writeln("")
    for entry_number, entry in pairs(container.entry_list) do
      if tostring(entry_number):sub(1,1) ~= '_' then
        if not self.production then
          f:writeln("if context.debug then")
          f:writeln("  print('  Entry: %s')", entry.parameter_ref or entry.container_ref)  
          f:writeln("end")
        end
        self:write_entry(f, container, entry_number, entry, false)
      end
    end    
  end

  -- entries contributed by child container which are required for
  -- the child determinitation (a.k.a read_ahead entries)
  if container.read_ahead_entry_list and #container.read_ahead_entry_list > 0 then    
    f:writeln("-- process entries contributed by child containers required for branching")  
    f:writeln("")
    f:writeln("local read_ahead_location_in_bits = location_in_bits")  
    f:writeln("")
    for entry_number, entry in pairs(container.read_ahead_entry_list) do      
      if not self.production then
        f:writeln("if context.debug then")
        f:writeln("  print('  read_aheadParameterEntry: %s')", entry.parameter_ref)  
        f:writeln("end")
      end
      self:write_entry(f, container, entry_number, entry, false, true)
    end    
  end

  -- child determination  
  if container.branches and next(container.branches) ~= nil then
    f:writeln("-- branch to the first child which conditions match the")
    f:writeln("-- current data")    
    f:writeln("")
    local first_branch = true
    
    for child, conditions in pairs(container.branches) do

      local n = table.length(conditions, function(k, v) return tostring(k):sub(1,1) ~= '_' end)
      local i = 1      
      f:indent()
      if first_branch then f:write('if ') else f:write('elseif ') end      

      for k, condition in pairs(conditions) do
        if tostring(k):sub(1,1) ~= '_' then 
          local parameter = self:get_parameter(condition.parameter_ref, container._path)
          local parameter_type = self:get_parameter_type(parameter.parameter_type_ref, container._path)
          
          local expr = ""
          local comment = ""
          condition.operator = condition.operator or "=="

          if parameter_type._type == "IntegerParameterType" then
            expr = string.format("values.%s %s %s", toidentifier(condition.parameter_ref), condition.operator, condition.value)
          
          elseif parameter_type._type == "EnumeratedParameterType" then            
            local label = condition.value          
            local value = self:get_enumerated_value(parameter.parameter_type_ref, label)
            comment = string.format("  -- %s", label)
            expr = string.format("values.%s %s %s", toidentifier(condition.parameter_ref), condition.operator, value)
          
          else
            errorf("Comparison to parameter of type %s in not supported in container %s",
                   parameter_type.name, container.name)
          end    

          local keyword = i == n and "then" or "and"
          
          if i == 1 then
            f:write("%s %s%s\n", expr, keyword, comment)
          else
            align_to_if_stmt = first_branch and "   " or "       "
            f:writeln("%s%s %s%s", align_to_if_stmt, expr, keyword, comment)
          end

          i = i + 1
        end
      end      
      f:writeln("  location_in_bits = %s(context, location_in_bits)", child)      
      first_branch = false
    end    
    f:writeln("end")
    f:writeln("")
  end

  -- sequence container tail entries
  if #container.tail_entry_list > 0 then
    f:writeln("-- process every entries in the tail of the container")
    f:writeln("")
    for entry_number, entry in pairs(container.tail_entry_list) do
      if not self.production then
        f:writeln("if context.debug then")
        f:writeln("  print('TailEntry: %s')",  entry.parameter_ref or entry.container_ref) 
        f:writeln("end")
      end
      self:write_entry(f, container, entry_number, entry, true)
    end
    f:writeln("")
  end

  f:writeln("-- return the new location")
  f:writeln("return location_in_bits", name, name)
  f:indent_less()
  f:writeln("end")
end

function SpaceSystemModel:generate_decommutation_model(f, root, production)  
  if not self:get_container(root) then
    errorf('Root sequence container "%s" does not exist', root)
  end

  local stream = Stream(f)
  self.production = production or false
  stream:writeln('local ffi = require "ffi"')
  stream:writeln("")
  self:write_enumerations(stream)
  self:write_calibrators(stream)
  self:write_root_fn(stream, root)
  for name, container in pairs(self.containers) do    
    self:write_container(stream, name, container)
    stream:writeln("")    
  end
  stream:writeln("return root")
end



return SpaceSystemModel