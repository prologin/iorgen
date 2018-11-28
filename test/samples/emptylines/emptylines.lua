-- empty_list: an empty list
-- buffer_string: here to check correct parsing of empty line above
-- n: an integer, will be 0 in the sample input
-- empty_in_sample: an empty list (only in the sample)
-- empty_string: an empty string
-- main: an other buffer string
-- empty_char_list: an empty char list
-- non_empty_char_list: an char list, non empty
-- struct_with_empty_line: a struct containing an empty line, then a struct
-- a_sized_struct: a sized struct containing an empty line
-- finish: a string to finish
function empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish)
    -- TODO Wow, lots of empty lines!
end

local empty_list = {}
for i in string.gmatch(io.read(), "-?%d+") do
    table.insert(empty_list, tonumber(i))
end
local buffer_string = io.read()
local n = tonumber(io.read())
local empty_in_sample = {}
for i in string.gmatch(io.read(), "-?%d+") do
    table.insert(empty_in_sample, tonumber(i))
end
local empty_string = io.read()
local main = io.read()
local empty_char_list = {}
io.read():gsub(".",function(i) table.insert(empty_char_list, i) end)
local non_empty_char_list = {}
io.read():gsub(".",function(i) table.insert(non_empty_char_list, i) end)
local struct_with_empty_line = {}
struct_with_empty_line["list in struct"] = {}
for i in string.gmatch(io.read(), "-?%d+") do
    table.insert(struct_with_empty_line["list in struct"], tonumber(i))
end
local i = {string.match(io.read(), "(%S) (-?%d+)")}
struct_with_empty_line["struct in struct"] = {char1 = i[1], int2 = tonumber(i[2])}
local a_sized_struct = {}
a_sized_struct["size"] = tonumber(io.read())
a_sized_struct["string in struct"] = io.read()
local finish = io.read()

empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish)
