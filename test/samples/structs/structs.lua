-- struct: a struct 1 instance
-- n: a number
-- struct_list: a list a struct 1
-- triangle: a triangle
-- struct_chars: a struct of chars
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
    local j = {string.match(io.read(), "(-?%d+) (-?%d+) (-?%d+)")}
    triangle[i]["pos"] = {x = tonumber(j[1]), y = tonumber(j[2]), z = tonumber(j[3])}
end
local i = {string.match(io.read(), "(%S) (%S) (%S)")}
local struct_chars = {["first char"] = i[1], ["second char"] = i[2], ["third char"] = i[3]}

structs(struct, n, struct_list, triangle, struct_chars)
