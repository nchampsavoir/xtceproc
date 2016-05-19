function table.print(t)
  for k, v in pairs(t) do
    print(string.format("%s = %s", k, v))
  end
end

function table.length(tbl, predicate)
  local count = 0
  if predicate then
    for key, val in pairs(tbl) do 
      if predicate(key, val) then
        count = count + 1 
      end
    end
  else
    for _ in pairs(tbl) do count = count + 1 end
  end
  return count
end

function string.snakecase(str)
  local out
  out = str:sub(1,1):lower()
  for i = 2, #str do
    local c = str:sub(i,i)
    if c == c:upper() then
      out = out .. "_"
      c = c:lower()
    end
    out = out .. c
  end
  return out
end

-- Compatibility: Lua-5.1
function string.split(str, pat)
   local t = {}  -- NOTE: use {n = 0} in Lua-5.0
   local fpat = "(.-)" .. pat
   local last_end = 1
   local s, e, cap = str:find(fpat, 1)
   while s do
      if s ~= 1 or cap ~= "" then
   table.insert(t,cap)
      end
      last_end = e+1
      s, e, cap = str:find(fpat, last_end)
   end
   if last_end <= #str then
      cap = str:sub(last_end)
      table.insert(t, cap)
   end
   return t
end

function string.split_path(str)
   return string.split(str,'[\\/]+')
end

function string.join(str, list)
  local s = ''
  local first = true
  for _, item in ipairs(list) do
    if first then
      s = s .. item
      first = false
    else
      s = s .. str .. item
    end
  end
  return s
end