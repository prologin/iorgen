-- struct (table: {"foo": number, "bar": number}): a struct 1 instance
-- n (number): a number
-- struct_list (table: array[{"foo": number, "bar": number}]): a list a struct 1
-- triangle (table: array[{"name": string, "description": string, "pos": {"x": number, "y": number, "z": number}}]): a triangle
-- struct_chars (table: {"first char": string, "second char": string, "third char": string}): a struct of chars
-- big_list_struct (table: {"int": number, "big list": array[array[array[number]]]}): the big list struct
function structs(struct, n, struct_list, triangle, struct_chars, big_list_struct)
    -- TODO Look at them structs.
end

local i = {string.match(io.read(), "(-?%d+) (-?%d+)")}
local struct = {foo = tonumber(i[1]), bar = tonumber(i[2])}
local n = tonumber(io.read())
local struct_list = {}
for i = 1, n do
    local j = {string.match(io.read(), "(-?%d+) (-?%d+)")}
    struct_list[i] = {foo = tonumber(j[1]), bar = tonumber(j[2])}
end
local triangle = {}
for i = 1, 3 do
    triangle[i] = {}
    triangle[i]["name"] = io.read()
    triangle[i]["description"] = io.read()
    local j = {string.match(io.read(), "(-?%d+) (-?%d+) (-?%d+)")}
    triangle[i]["pos"] = {x = tonumber(j[1]), y = tonumber(j[2]), z = tonumber(j[3])}
end
local i = {string.match(io.read(), "(%S) (%S) (%S)")}
local struct_chars = {["first char"] = i[1], ["second char"] = i[2], ["third char"] = i[3]}
local big_list_struct = {}
big_list_struct["int"] = tonumber(io.read())
big_list_struct["big list"] = {}
for i = 1, 2 do
    big_list_struct["big list"][i] = {}
    for j = 1, 2 do
        big_list_struct["big list"][i][j] = {}
        for k in string.gmatch(io.read(), "-?%d+") do
            table.insert(big_list_struct["big list"][i][j], tonumber(k))
        end
    end
end

structs(struct, n, struct_list, triangle, struct_chars, big_list_struct)
