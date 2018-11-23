-- n: the size of the lists
-- lists: a list of list of different sizes
-- strings: a list of strings of different sizes
-- matrices: a list of matrices of different sizes
-- same: a list of list of same sizes
function sized_struct(n, lists, strings, matrices, same)
    -- TODO The is a special case.
end

local n = tonumber(io.read())
local lists = {}
for i = 1, n do
    lists[i] = {}
    lists[i]["size1"] = tonumber(io.read())
    lists[i]["int list"] = {}
    for j in string.gmatch(io.read(), "-?%d+") do
        table.insert(lists[i]["int list"], tonumber(j))
    end
end
local strings = {}
for i = 1, n do
    strings[i] = {}
    strings[i]["size2"] = tonumber(io.read())
    strings[i]["string list"] = io.read()
end
local matrices = {}
for i = 1, 2 do
    matrices[i] = {}
    matrices[i]["size3"] = tonumber(io.read())
    matrices[i]["list list"] = {}
    for j = 1, matrices[i]["size3"] do
        matrices[i]["list list"][j] = {}
        for k in string.gmatch(io.read(), "-?%d+") do
            table.insert(matrices[i]["list list"][j], tonumber(k))
        end
    end
end
local same = {}
for i = 1, n do
    same[i] = {}
    same[i]["size4"] = tonumber(io.read())
    same[i]["int list n"] = {}
    for j in string.gmatch(io.read(), "-?%d+") do
        table.insert(same[i]["int list n"], tonumber(j))
    end
end

sized_struct(n, lists, strings, matrices, same)
