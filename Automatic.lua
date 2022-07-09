Auto = {}
AutoUI = {}

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Arithmetic-----------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Logistic function, Can be used for juicy UI among other things.
---https://www.desmos.com/calculator/cmmwrjtyit?invertedColors
---@param v number
---@param max number
---@param steep number
---@param offset number
---@return number
function Auto.Logistic(v, max, steep, offset)
	return max / (1 + math.exp((v - offset) * steep))
end

---'n' is the number you want rounded, 'd' is the decimal, so something like 0.1, 0.01, 1, 2, 100, 2.5
---Challenge by @TallTim and @1ssnl to make the smallest rounding function.
---@param n number
---@param d number
---@return number
function Auto.Round(n,d)x=1/d return math.floor(n*x+.5)/x end

---Limits a value from going below the min and above the max
---@param val number
---@param min number
---@param max number
---@return number
function Auto.Clamp(val, min, max)
	if val < min then
		return min
	elseif val > max then
		return max
	else
		return val
	end
end

function Auto.Biggest(...)
	local best = -math.huge
	for i in pairs(arg) do
		if i > best then best = i end
	end
	return best
end

function Auto.Smallest(...)
	local best = math.huge
	for i in pairs(arg) do
		if i < best then best = i end
	end
	return best
end

---Return a Random Vector
---@param length any
---@param precision any
---@return table
function Auto.RndVec(length, precision)
	precision = precision or 0.01
	local m = 1/precision
	local v = VecNormalize(Vec(math.random(-m,m), math.random(-m,m), math.random(-m,m)))
	return VecScale(v, length)	
end

---Lerp function, Is not clamped meaning it if t is above 1 then it will 'overshoot'
---@param a number
---@param b number
---@param t number
---@return number
function Auto.LerpUnclamped(a, b, t)
	return (1-t)*a + t*b
end

--- Moves a towards b by t
---@param a number
---@param b number
---@param t number
---@return number
function Auto.Move(a, b, t)
	output = a
    if a == b then
		return a
	end

	if a > b then
		output = math.max(a - t, b)
	else
		output = math.min(a + t, b)
	end

	return output
end

function Auto.Map(x, rangemin, rangemax, newrangemin, newrangemax)
	return newrangemin + ((newrangemax - rangemin) / (rangemax - rangemin)) * (x - rangemin)
end

---Return the Distance between Two Vectors
---@param a Vec
---@param b Vec
---@return number
function Auto.VecDist(a, b)
	return math.sqrt( (a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2 )
end

---Return the Distance between the numbers a and b
---@param a number
---@param b number
---@return number
function Auto.Dist(a, b)
	return math.abs(a - b)
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Utility-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Counts the amount of elements in a list
---@param t table
---@return integer
function Auto.Count(t)
	local c = 0
	for i in pairs(t) do
		c = c + 1
	end

	return c
end

function Auto.Last(t)
	return t[Auto.Count(t)]
end

function Auto.Pack(...)
	-- Returns a new table with parameters stored into an array, with field "n" being the total number of parameters
	local t = { ... }
	t.n = #t
	return t
end

---A workaround to making a table readonly
---@param tbl table
---@return table
function Auto.SetReadOnly(tbl)
	return setmetatable({}, {
		__index = tbl,
		__newindex = function(t, key, value)
			error("attempting to change constant " ..
				tostring(key) .. " to " .. tostring(value), 2)
		end
	})
end

function Auto.ToString(...)
	local output = ""
	local count = Auto.Count(arg)

	local iterator = 1
	for index in pairs(arg) do
		local value = arg[index]
		if output ~= "" then output = output .. " : " end
		
		local t = type(value)
		if t == 'nil' then
			output = output .. "nil"
		elseif t ~= 'table' then
			output = output .. tostring(value)
		else
			output = output .. Auto.ToString(unpack(value))
		end
		
		iterator = iterator + 1
		if iterator == count then break end
	end

	return output
end

function Auto.Print(...)
	DebugPrint(Auto.ToString(unpack(arg)))
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Game-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function Auto.DrawOutlineBlink(entity, speed, time, red, green, blue, alphamulti)
	speed = speed or 1
	time = time or GetTime()
	red = red or 1
	green = green or 1
	blue = blue or 1
	alphamulti = alphamulti or 1

	local t = (time * speed) % 2
	local alpha = Auto.Logistic(t, 1, -15, 0.4) * Auto.Logistic(t, 1, 15, 1.4)

	local type = GetEntityType(entity)
	if type == 'body' then
		DrawBodyOutline(entity, red, green, blue, alpha * alphamulti)
	elseif type == 'shape' then
		DrawShapeOutline(entity, red, green, blue, alpha * alphamulti)
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

AutoUI.Pad = Auto.SetReadOnly({none = 0, atom = 4, micro = 6, thin = 12, thick = 24, heavy = 48, beefy = 120})

function AutoUI.AlignmentToPos(alignment)
	x, y = 0, 0
	if string.find(alignment, 'left') then x = -1 end
	if string.find(alignment, 'center') then x = 0 end
	if string.find(alignment, 'right') then x = 1 end
	if string.find(alignment, 'bottom') then y = -1 end
	if string.find(alignment, 'middle') then y = 0 end
	if string.find(alignment, 'top') then y = 1 end
	return {x = x, y = y}
end

function AutoUI.Center()
	UiTranslate(UiCenter(), UiMiddle())
	UiAlign('center middle')
end

-- function AutoUI.PushDynamicContainer(callback, alignment, padding, spacing, draw)
-- 	table.insert(Stack, {
-- 		params = {
-- 			alignment = alignment,
-- 			padding = padding or AutoUI.Pad.thick,
-- 			spacing = spacing or AutoUI.Pad.thick,
-- 			draw = draw or true,
-- 		},
-- 		contains = {},
-- 		callback = callback or nil
-- 	})

-- 	UiPush()
-- end

-- function AutoUI.Button(callback, text, fontsize, paddingwidth, paddingheight)
-- 	if Auto.Count(Stack) ~= 0 then
-- 		table.insert(Auto.Last(Stack).contains, {
-- 			func = AutoUI.Create.Button,
-- 			params = { text, fontsize, paddingwidth, paddingheight },
-- 			callback = callback or nil
-- 		})
-- 	else
-- 		AutoUI.Create.Button(true, text, fontsize, paddingwidth, paddingheight)
-- 	end
-- end

-- function AutoUI.PopDynamicContainer()
-- 	if Auto.Count(Stack) == 0 then error("No Container to Pop") end
-- 	local container = Auto.Last(Stack)
-- 	local count = Auto.Count(container.contains)
-- 	local space = {w = 0, h = 0}

-- 	local fakecallbacks = {} 
-- 	local callbacks = {}
	
-- 	UiPush()
-- 		local largest = {w = 0, h = 0}
-- 		for i, v in ipairs(container.contains) do
-- 			fake = v.func(false, unpack(v.params))
-- 			fakecallbacks[i] = fake
-- 			if fake.rect.w > largest.w then largest.w = fake.rect.w end
-- 			if fake.rect.h > largest.h then largest.h = fake.rect.h end
-- 			space.h = space.h + fake.rect.h
-- 		end

-- 		space.h = space.h + (count - 1) * container.params.spacing
-- 	UiPop()

-- 	UiPush()
-- 		container.size = { w = largest.w, h = space.h }
-- 		container.out = AutoUI.Create.Container(container.size.w, container.size.h, container.params.alignment, container.params.padding, container.params.draw)

-- 		UiAlign('center middle')
		
-- 		for i, v in ipairs(container.contains) do
-- 			UiPush()
-- 				local pos = {
-- 					x = UiCenter(),
-- 					y = Auto.Map(i, 1, count, fakecallbacks[1].rect.h / 2 + container.params.padding / 2, space.h - fakecallbacks[1].rect.h)
-- 				}
-- 				UiTranslate(pos.x, pos.y)
-- 				real = v.func(true, unpack(v.params))

-- 				if v.callback ~= nil then
-- 					callbacks[v.callback] = real
-- 				end
-- 			UiPop()
-- 		end
-- 	UiPop()

-- 	UiPop()

-- 	return { hover = container.out.hover, rect = {w = container.size.w, h = container.size.h}, callbacks = callbacks }
-- end

local Stack = {}

function AutoUI.SpreadDown(padding)
	table.insert(Stack, {type = 'spread', direction = 'down', padding = padding or AutoUI.Pad.thin})
	UiPush()
end

function AutoUI.SpreadRight(padding)
	table.insert(Stack, {type = 'spread', direction = 'right', padding = padding or AutoUI.Pad.thin})
	UiPush()
end

function AutoUI.SpreadVerticle(count)
	table.insert(Stack, {type = 'spread', direction = 'verticle', length = UiHeight(), count = count})
	UiPush()
end

function AutoUI.SpreadHorizontal(count)
	table.insert(Stack, {type = 'spread', direction = 'horizontal', length = UiWidth(), count = count})
	UiPush()
end

function GetSpread()
	local count = Auto.Count(Stack)
	for i = count, 1, -1 do
		if Stack[i].type == 'spread' then
			return Stack[i]
		end
	end
	error('No Previous Spread Found')
	return nil
end

function AutoUI.SpreadEnd()
	while true do
		local count = Auto.Count(Stack)
		if Stack[count].type ~= 'spread' then
			table.remove(Stack, count)
		else
			table.remove(Stack, count)
			UiPop()
			return
		end
		if count <= 1 then return end
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface Creation Functions-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoUI.Container(width, height, alignment, padding, draw)
	width = width or 300
	height = height or 400
	alignment = alignment or 'left top'
	padding = math.max(padding or AutoUI.Pad.micro, 0)
	draw = draw or true

	local paddingwidth = width + padding * 2
	local paddingheight = height + padding * 2
	
	UiAlign(alignment)
	UiWindow(paddingwidth, paddingheight, false)

	UiAlign('left top')
	if draw then
		UiColor(0, 0, 0, 0.55)
		UiImageBox("ui/common/box-solid-10.png", UiWidth(), UiHeight(), 10, 10)
	end

	hover = UiIsMouseInRect(UiWidth(), UiHeight())
	
	UiTranslate(padding, padding)
	UiWindow(width, height, false)

	local offset = {x = 0, y = 0}

	UiTranslate(offset.x, offset.y)
	
	return { rect = { w = width, h = height }, hover = hover }
end

function AutoUI.Button(name, fontsize, paddingwidth, paddingheight, draw)
	fontsize = fontsize or 28
	paddingwidth = paddingwidth or AutoUI.Pad.heavy
	paddingheight = paddingheight or AutoUI.Pad.thin
	draw = draw or true

	UiPush()
		UiWordWrap(UiWidth() - AutoUI.Pad.thick)
		UiFont("regular.ttf", fontsize)

		UiColor(0, 0, 0, 0)
		local rw, rh = UiText(name)
		local padrw, padrh = rw + paddingwidth, rh + paddingheight
		
		if draw then
			hover = UiIsMouseInRect(padrw, padrh)
			if hover then UiColorFilter(1, 1, 0.55, 1) end
			UiColor(1, 1, 1, 1)
			
			UiButtonImageBox('ui/common/box-outline-6.png', 6, 6)
			pressed = UiTextButton(name, padrw, padrh)
		end
	UiPop()

	local data = { pressed = pressed, hover = hover, rect = { w = padrw, h = padrh } }
	table.insert(Stack, {type = 'button', data = data})

	local gs = GetSpread()

	if gs ~= nil then
		if gs.direction == 'down' then
			UiTranslate(0, data.rect.h + gs.padding)
		elseif gs.direction == 'right' then
			UiTranslate(data.rect.w + gs.padding, 0)
		elseif gs.direction == 'verticle' then
			UiTranslate(0, gs.length / gs.count * 1)
		elseif gs.direction == 'horizontal' then
			UiTranslate(gs.length / gs.count * 1, 0)
		end
	end

	return pressed, data
end

function AutoUI.Slider(set, min, max, lockincrement, paddingwidth, paddingheight)
	min = min or 0
	max = max or 1
	set = set or (min + max / 2)
	lockincrement = lockincrement or 0
	paddingwidth = paddingwidth or AutoUI.Pad.thick
	paddingheight = paddingheight or AutoUI.Pad.thin

	local width = UiWidth() - paddingwidth * 2
	local dotwidth, dotheight = UiGetImageSize("ui/common/dot.png")

	set = Auto.Map(set, min, max, 0, width)


	UiPush()
		UiTranslate(paddingwidth, paddingheight)
		UiColor(1, 1, 0.5, 1)
	
		UiPush()
			UiTranslate(0, dotheight / 2)
			UiRect(width, 2)
		UiPop()
			
		UiTranslate(-dotwidth / 2, 0)

		-- UiTranslate(-UiGetSliderDot().rect.w / 2, -UiGetSliderDot().rect.h / 2)
		-- elapsedheight = elapsedheight + UiGetSliderDot().rect.h / 2

		set, released = UiSlider('ui/common/dot.png', "x", set, 0, width)
		set = Auto.Map(set, 0, width, min, max)
		set = Auto.Round(set, lockincrement)
	UiPop()

	local data = { value = set, released = released, rect = {w = width, h = paddingheight * 2} }

	local gs = GetSpread()

	if gs ~= nil then
		if gs.direction == 'down' then
			UiTranslate(0, data.rect.h + gs.padding)
		elseif gs.direction == 'right' then
			UiTranslate(data.rect.w + gs.padding, 0)
		elseif gs.direction == 'verticle' then
			UiTranslate(0, gs.length / gs.count * 1)
		elseif gs.direction == 'horizontal' then
			UiTranslate(gs.length / gs.count * 1, 0)
		end
	end

	return set, data
	-- return { value = value, hover = hover, slider = hoverslider, rect = { w = rw, h = elapsedheight } }
end

function AutoUI.Marker(size)
	size = (size or 1) / 2
	UiPush()
		UiAlign('center middle')
		UiScale(size, size)
		UiColor(1, 1, 0.55, 1)
		UiImage('ui/common/dot.png')
	UiPop()
end

function AutoUI.Tooltip(text, position, fontsize, alpha, bold)
	text = text or "nil"
	fontsize = fontsize or 24
	alpha = alpha or 0.75
	bold = bold or false

	UiPush()
	UiAlign('center middle')
	local x, y = UiWorldToPixel(position)
	UiTranslate(x, y)
	UiWordWrap(UiMiddle())

	-- UiScale(scale)

	UiFont(bold and "bold.ttf" or "regular.ttf", fontsize)
	UiColor(0, 0, 0, 0)
	local rw, rh = UiText(text)

	UiColor(0, 0, 0, alpha)
	UiRect(rw, rh)

	UiColor(1, 1, 1, alpha)
	UiText(text)
	UiPop()
end
