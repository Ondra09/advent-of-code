#!/usr/bin/lua

local open = io.open

local function read_file(path)
    local file = open(path, "rb") -- r read mode and b binary mode
    if not file then return nil end
    local content = file:read "*a" -- *a or *all reads the whole file
    file:close()
    return content
end

local fileContent = read_file("input-test");
print (fileContent);

for c in fileContent:gmatch"." do
    -- do something with c
    print (c)
end
