local argparse = require "3rdparty.argparse"
local Buffer = require "3rdparty.buffer"

local parser = argparse("concat", "Make a big file by concatenating multiple copies of a small file")
parser:argument("inputfile")
parser:option("-o --outputfile")
parser:option("-s --size", "Target size in megabytes", "50")

local args = parser:parse()
args.outputfile = args.outputfile or args.inputfile .. ".big"

local inputfile, err = io.open(args.inputfile, "rb")
if inputfile == nil then
    print(err)
    os.exit()
end

local content = inputfile:read("*all")
inputfile:close()

local outputfile, err = io.open(args.outputfile, "wb")
if outputfile == nil then
    print(err)
    os.exit()
end

local content_size = string.len(content)
local target_size = args.size * 1000 * 1000
local s = 0

while s < target_size do
    outputfile:write(content)
    s = s + content_size
end

outputfile:close()
