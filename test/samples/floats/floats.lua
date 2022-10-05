-- f (number): a float
-- g (number): a float, greater than f
-- point (table: {"x": number, "y": number, "z": number}): some coordinates
-- n (number): a number
-- float_list (table: array[number]): a list of floats
-- other_list (table: array[number]): a list of floats
-- inlined (table: array[{"integer": number, "char": string, "float": number}]): some inlined structs
-- multiline (table: {"integer 2": number, "string": string, "float 2": number}): a multiline struct
function floats(f, g, point, n, float_list, other_list, inlined, multiline)
    -- TODO Parsing is often easy, reprint mode is harder
end

local f = tonumber(io.read())
local g = tonumber(io.read())
local i = {string.match(io.read(), "(-?%g+) (-?%g+) (-?%g+)")}
local point = {x = tonumber(i[1]), y = tonumber(i[2]), z = tonumber(i[3])}
local n = tonumber(io.read())
local float_list = {}
for i in string.gmatch(io.read(), "-?%g+") do
    table.insert(float_list, tonumber(i))
end
local other_list = {}
for i in string.gmatch(io.read(), "-?%g+") do
    table.insert(other_list, tonumber(i))
end
local inlined = {}
for i = 1, 3 do
    local j = {string.match(io.read(), "(-?%d+) (%S) (-?%g+)")}
    inlined[i] = {integer = tonumber(j[1]), char = j[2], float = tonumber(j[3])}
end
local multiline = {}
multiline["integer 2"] = tonumber(io.read())
multiline["string"] = io.read()
multiline["float 2"] = tonumber(io.read())

floats(f, g, point, n, float_list, other_list, inlined, multiline)
