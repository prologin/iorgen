-- struct (table: {"foo": number, "bar": number}): a struct 1 instance
-- n (number): a number
-- struct_list (table: array[{"foo": number, "bar": number}]): a list a struct 1
-- triangle (table: array[{"name": string, "description": string, "pos": {"x": number, "y": number, "z": number}}]): a triangle
-- struct_chars (table: {"first char": string, "second char": string, "third char": string}): a struct of chars
function structs(struct, n, struct_list, triangle, struct_chars)
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

structs(struct, n, struct_list, triangle, struct_chars)
