-- n: a number, used as a size
-- list: a list of structs
function example(n, list)
    -- TODO In a real life scenario, you will describe here what you want the
    -- end user to do with this generated code
end

local n = tonumber(io.read())
local list = {}
for i = 1, n do
    local j = {string.match(io.read(), "(-?%d+) (%S)")}
    list[i] = {integer = tonumber(j[1]), character = j[2]}
end

example(n, list)
