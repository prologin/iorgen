-- n (number): the first list's size
-- list_int (table: array[number]): a list containing ints
-- size (number): an other size
-- list_char (table: array[string]): a list of char
-- string_ (string): a string
-- list_string4 (table: array[string]): a list of strings of size 4
-- list_list_string2 (table: array[array[string]]): a list of list of strings of size 2 of size 2 of size 2
-- matrix (table: array[array[number]]): a matrix of int
function lists(n, list_int, size, list_char, string_, list_string4, list_list_string2, matrix)
    -- TODO Aren't these lists beautifull?
end

local n = tonumber(io.read())
local list_int = {}
for i in string.gmatch(io.read(), "-?%d+") do
    table.insert(list_int, tonumber(i))
end
local size = tonumber(io.read())
local list_char = {}
io.read():gsub(".",function(i) table.insert(list_char, i) end)
local string_ = io.read()
local list_string4 = {}
for i = 1, size do
    list_string4[i] = io.read()
end
local list_list_string2 = {}
for i = 1, 2 do
    list_list_string2[i] = {}
    for j = 1, 2 do
        list_list_string2[i][j] = io.read()
    end
end
local matrix = {}
for i = 1, size do
    matrix[i] = {}
    for j in string.gmatch(io.read(), "-?%d+") do
        table.insert(matrix[i], tonumber(j))
    end
end

lists(n, list_int, size, list_char, string_, list_string4, list_list_string2, matrix)
