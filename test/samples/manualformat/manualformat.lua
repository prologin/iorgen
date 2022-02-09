-- a (number): a first number
-- b (number): a second number
-- c (number): a third number
-- n (number): This one on a new line
-- one_per_line (table: array[number]): an integer list, one per line
function manual_format(a, b, c, n, one_per_line)
    -- TODO From the function perspective, this is just 4 integers
end

local a, b, c = string.match(io.read(), "(-?%d+) (-?%d+) (-?%d+)")
local n = tonumber(io.read())
local one_per_line = {}
for i = 1, 3 do
    one_per_line[i] = tonumber(io.read())
end

manual_format(a, b, c, n, one_per_line)
