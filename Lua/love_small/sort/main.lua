--[[
Visual representation of quicksort algorithm using Love2d.

Flavours of this implementation:
  - switching to insertion sort on small partition size
  - pivot is a median of first, last and middle elements of the partition

References used:
https://en.wikipedia.org/wiki/Quicksort
https://rosettacode.org/wiki/Sorting_algorithms/Quicksort
--]]

MAX_VAL = 2000000
LIST_SIZE = 100000
ISORT_THRESHOLD = 32    -- switch to insertion sort if partition is less than this
STEP_SIZE = 250         -- redraw list after each STEP_SIZE swap

function love.load()
    screen_width, screen_height = love.graphics.getDimensions()
    fontConsole = love.graphics.newFont(16)
    love.graphics.setFont(fontConsole)
    step = 0
    reads, writes = 0, 0
    L = make_list(MAX_VAL, LIST_SIZE)
    x_ratio = screen_width/LIST_SIZE
    y_ratio = screen_height/MAX_VAL
    val_colors, rnd_colors, ref_values = get_color_tables(L)
    firstTime = true
    startSorting = false
    sort_co = coroutine.create(qsort)
end

function love.draw()
    draw_list(L)
    if startSorting then
        if firstTime then
            coroutine.resume(sort_co, L, 1, #L)
        else
            firstTime = false
            coroutine.resume(sort_co)
        end
    end
end

function love.keypressed(key, scancode, isrepeat)
    if key == "escape" then love.event.quit() end
    if key == "space" then startSorting = true end
end

-- ################ Sort ################

function qsort(L, lo, hi)
    if hi-lo < ISORT_THRESHOLD then
        isort(L, lo, hi)
    else
        left, right = partition(L, lo, hi)
        qsort(L, lo, right)
        qsort(L, left, hi)
    end
end

function partition(L, lo, hi)
    mid = lo+math.floor((hi-lo)/2)
    pivot = median3({L[lo], L[mid], L[hi]})
    reads = reads+3
    while true do
        while L[lo] < pivot do lo = lo+1; reads = reads+1 end
        while pivot < L[hi] do hi = hi-1; reads = reads+1 end
        reads = reads+2
        if lo >= hi then return lo, hi end
        L[lo], L[hi] = L[hi], L[lo]
        lo = lo+1; hi = hi-1
        writes = writes+2
        -- yield to redraw the list each STEP_SIZE
        step = step+1
        if step > STEP_SIZE then
            step = 0
            coroutine.yield()
        end
    end
end

-- ################ Auxiliary ################

function isort(L, lo, hi)
    for i = lo, hi do
        tmp = L[i]
        reads = reads+1
        j = i
        while j > lo and tmp < L[j-1] do
            L[j] = L[j-1]
            j = j-1
            reads, writes = reads+1, writes+1
        end
        L[j] = tmp
        reads, writes = reads+1, writes+1
    end
end

function make_list(max_val, list_size)
    res = {}
    math.randomseed(math.floor(os.clock() * 100000))
    for _ = 1, list_size do
        table.insert(res, math.random(max_val))
    end
    return res
end

function median3(L)
    if L[1] > L[2] then L[1], L[2] = L[2], L[1] end
    if L[2] > L[3] then L[2], L[3] = L[3], L[2] end
    if L[1] > L[2] then return L[1] end
    return L[2]
end

function draw_list(L)
    for i, v in ipairs(L) do
        if v == ref_values[i] then
            c = val_colors[v]
        else
            c = rnd_colors[v]
        end
        love.graphics.setColor(c.r, c.g, c.b)
        love.graphics.points(i*x_ratio, screen_height-(v*y_ratio))
    end
    love.graphics.setColor(255, 255, 255)
    love.graphics.print('Quick Sort of '..LIST_SIZE..' integers: '..reads..' reads, '..writes..' writes' , 10, 10)
end

function get_color(x, color_th)
    if x < color_th[1] then
        gradient = math.floor(x*color_th[4])
        r = 255-gradient
        g = gradient
        b = 255
    elseif x < color_th[2] then
        gradient = math.floor((x-color_th[1])*color_th[4])
        r = 0
        g = 255
        b = 255-gradient
    elseif x < color_th[3] then
        gradient = math.floor((x-color_th[2])*color_th[4])
        r = gradient
        g = 255
        b = 0
    else
        gradient = math.floor((x-color_th[3])*color_th[4])
        r = 255
        g = 255
        b = gradient
    end
    return {r = r, g = g, b = b}
end

function get_color_thresholds()
    c_th = {}
    table.insert(c_th, math.floor(MAX_VAL/4))        -- 25%
    table.insert(c_th, math.floor(MAX_VAL/2))        -- 50%
    table.insert(c_th, math.floor(MAX_VAL/4)*3)      -- 75%
    table.insert(c_th, 255/(MAX_VAL/4)) -- a step of color gradient
    return c_th
end

function get_color_tables(L)
    --[[ Generates 3 lookup tables:
    1. Colors corellate to values
    2. Colors are random
    3. Reference table in form of { value = its final position } ]]
    val_c, rnd_c, ref, L_ = {}, {}, {}, {}
    color_th = get_color_thresholds()
    for i, v in ipairs(L) do table.insert(L_, v) end
    table.sort(L_)
    for i, v in ipairs(L_) do
        if not val_c[v] then
            val_c[v] = get_color(v, color_th) -- color by value
        end
        if not rnd_c[v] then
            lo = math.max(0, v-color_th[2])
            hi = math.min(MAX_VAL, v+color_th[2])
            rnd_c[v] = get_color(math.random(lo, hi), color_th) -- random color
        end
        ref[i] = v
    end
    return val_c, rnd_c, ref
end
