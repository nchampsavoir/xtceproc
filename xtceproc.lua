local argparse = require "3rdparty.argparse"
local Buffer = require "3rdparty.buffer"
local SpaceSystemModel = require "ssm"
local Decom = require "decom"


function errorf(fmt, ...)
  print(string.format(fmt, ...) .. '\n')  
  os.exit(1)
end

function warnf(fmt, ...)
  print(string.format("WARNING: " .. fmt, ...))  
end

function printf(fmt, ...)
  print(string.format(fmt, ...))  
end


-------------------------------------------------------
-- Compiles an XTCE file into a Lua decommutation model
--

function compile(args)
  local start_time = os.clock()
  local file, err = io.open(args.xtcefile, "r")
  if file == nil then
    print(err)
    exit()
  end

  local content = file:read("*all")
  file:close()

  local ssm = SpaceSystemModel:new()
  ssm:load_xtce(content)

  args.outputfile = args.outputfile or args.xtcefile .. ".lua"
  local file, err = io.open(args.outputfile, "w")
  if file == nil then
    print(err)
    exit()
  end
  ssm:generate_decommutation_model(file, args.root, args.production)
  file:close()

  local end_time = os.clock()
  local elapsed_time = end_time - start_time
  if args.verbose then
    printf("Decommutation model successfully generated in %.02f seconds.", elapsed_time)
  end
end


----------------------------------------
-- Lists the content of a telemetry file
--

function list(args)
  local decom = Decom(model, {pus=args.pus, debug=args.debug})

  local tmfile, err = io.open(args.tmfile, "rb")
  if tmfile == nil then
      print(err)
      os.exit()
  end

  local block_size = 2^16
  local total_bytes = 0
  local start_time = os.clock()

  -- for every packet in the input stream
  while true do

    -- if there is still data left in the input file push them to
    -- the decom internal buffer
    block = tmfile:read(block_size)        
    if block == nil then break end -- EOF    
    decom:append(block)
    
    -- for every packet in the decom buffer
    while decom:has_more() do
      -- if packet is not valid, bail out
      if not decom:packet_is_valid() then
          print ("Invalid Packet detected.")            
          os.exit(-1)
      end        

      total_bytes = total_bytes + decom.packet_length
      decom:read_headers()
      decom:next()       
    end
  end

  tmfile:close()
  local end_time = os.clock()
  local elapsed_time = end_time - start_time
  local speed = math.floor(total_bytes / elapsed_time / 1000)  

  if args.apid then
    for key, value in pairs(decom.apids) do 
      printf("APID %s:\t%d", key, value)      
    end    
  elseif args.pus then
    for key, value in pairs(decom.pus_services) do 
      printf("SERVICE %s:\t%d", key, value)      
    end    
  end
  printf("TOTAL PACKETS: %d", decom.total_packets)
  
  if args.verbose then
    printf("%d kbytes processed in %.02f seconds. (%d KB/s)", math.floor(total_bytes / 1000), elapsed_time, speed)
  end
end


-------------------------------------------
-- Extracts the content of a telemetry file
--

function extract(args)
  local decom = Decom(args.modelfile, {pus=args.pus, debug=args.debug})

  local tmfile, err = io.open(args.tmfile, "rb")
  if tmfile == nil then
      print(err)
      os.exit()
  end

  local outputfile
  if not args.benchmark then
      args.output = args.output or args.tmfile .. ".txt"
      outputfile, err = io.open(args.output, "w")
      if outputfile == nil then
          print(err)
          os.exit()
      end
  end

  local block_size = 2^16
  local total_bytes = 0
  local total_values = 0
  local start_time = os.clock()

  -- for every packet in the input stream
  while true do

    -- if there is still data left in the input file push them to
    -- the decom internal buffer
    block = tmfile:read(block_size)        
    if block == nil then break end -- EOF    
    decom:append(block)
    
    -- for every packet in the decom buffer
    while decom:has_more() do

      -- if packet is not valid, bail out
      if not decom:packet_is_valid() then
        print ("Invalid Packet detected.")            
        os.exit(-1)
      end
      total_bytes = total_bytes + decom.packet_length
      
      -- decommute each parameter in th packet using the decom engine
      for mnemo, raw_value, calibrated_value in decom:iter_values() do            
        total_values = total_values + 1
        if outputfile then 
          -- serialize the parameter values to json 
          local s = string.format('%s\t%s\t%s\n', mnemo, raw_value, calibrated_value)
          -- write the serialized json to disk
          outputfile:write(s) 
        end            
      end
      
      decom:next()
    end
  end

  tmfile:close()
  if outputfile then outputfile:close() end

  local end_time = os.clock()
  local elapsed_time = end_time - start_time
  local byte_speed = math.floor(total_bytes / elapsed_time / 1000)  
  local value_speed = math.floor(total_values / elapsed_time)  

  if args.verbose then
    printf("%d kbytes processed in %.02f seconds. (%d KB/s)", math.floor(total_bytes / 1000), elapsed_time, byte_speed)
    printf("%d parameter values extracted. (%d values/s)", total_values, value_speed)
  end
end


----------------------------------------
-- Parse Command Line Arguments
--

local parser = argparse("xtceproc", "XTCE-based spacecraft telemetry processor")

-- Compile XTCE
local compile_parser = parser:command("compile i", "Compiles an XTCE file into a Lua decommutation model")
compile_parser:argument("xtcefile")
compile_parser:option("-r --root", "Root Sequence Container", "/GENERIC/CUSTOM/ROOT_TM_CCSDSPACKET")
compile_parser:option("-o --outputfile", "")
compile_parser:flag("-p --production", "Strips the generated file of every debug information")
compile_parser:flag("-v --verbose", "Outputs extra information like processing time")

-- List the content of a telemtry file
local list_parser = parser:command("list l", "Lists the content of a telemetry file")
list_parser:argument("tmfile")
list_parser:flag("-a --apid", "Display number of packets per APID")
list_parser:flag("-p --pus", "Display number of packets per PUS Service/Subservice")
list_parser:flag("-d --debug", "Debug mode")
list_parser:flag("-v --verbose", "Outputs extra information like processing time")

-- Extract the parameter values from a TM file using a decommuation model
local extract_parser = parser:command("extract e", "Extracts the parameter values from a telemetry file")
extract_parser:argument("modelfile")
extract_parser:argument("tmfile")
extract_parser:flag("-d --debug", "Debug mode")
extract_parser:flag("-b --benchmark", "Disable output to measure the library performance")
extract_parser:flag("-v --verbose", "Outputs extra information like processing time")

local args = parser:parse()

if args.compile then compile(args) end
if args.list then list(args) end
if args.extract then extract(args) end


