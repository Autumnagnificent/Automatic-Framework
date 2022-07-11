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
---This was a Challenge by @TallTim and @1ssnl to make the smallest rounding function, but I expanded it to make it easier to read and a little more efficent
---@param n number
---@param d number
---@return number
function Auto.Round(n,d)
	d = Auto.Default(d, 0.1)
	if d == 0 then return n end
	x = 1 / d
	return math.floor(n * x + 0.5) / x
end

---Limits a value from going below the min and above the max
---@param val number
---@param min number
---@param max number
---@return number
function Auto.Clamp(val, min, max)
	min = Auto.Default(min, 0)
	max = Auto.Default(max, 1)
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
	precision = Auto.Default(precision, 0.01)
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

function Auto.Map(x, a1, a2, b1, b2)
	return b1 + ((x - a1) * (b2 - b1)) / (a2 - a1)
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

function Auto.Default(val, default)
	if val == nil then return default else return val end
end

---A workaround to making a table readonly
---@param tbl table
---@return table
function Auto.SetReadOnly(tbl)
	return setmetatable({}, {
		__index = tbl,
		__newindex = function(t, key, value)
			error("attempting to change constant " .. tostring(key) .. " to " .. tostring(value), 2)
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

function Auto.RetrievePath(precision)
	precision = Auto.Default(precision, 0.2)
	
	local path = {}
	local length = GetPathLength()
	local l = 0
	while l < length do
		path[#path + 1] = GetPathPoint(l)
		l = l + precision
	end

	return path, path[#path]
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Game-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function Auto.DrawOutlineBlink(entity, speed, time, red, green, blue, alphamulti)
	speed = Auto.Default(speed, 1)
	time = Auto.Default(time, GetTime)()
	red = Auto.Default(red, 1)
	green = Auto.Default(green, 1)
	blue = Auto.Default(blue, 1)
	alphamulti = Auto.Default(alphamulti, 1)

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

AutoUI.Pad = {none = 0, atom = 4, micro = 6, thin = 12, thick = 24, heavy = 48, beefy = 120}
AutoUI.PrimaryColor = {0.95, 0.95, 0.95, 1}
AutoUI.SpecialColor = {1, 1, 0.55, 1}
AutoUI.SecondaryColor = {0, 0, 0, 0.55}
local Stack = {}


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

function AutoUI.SpreadDown(padding)
	table.insert(Stack, {type = 'spread', direction = 'down', padding = Auto.Default(padding, AutoUI.Pad.thin)})
	UiPush()
end

function AutoUI.SpreadUp(padding)
	table.insert(Stack, {type = 'spread', direction = 'up', padding = Auto.Default(padding, AutoUI.Pad.thin)})
	UiPush()
end

function AutoUI.SpreadRight(padding)
	table.insert(Stack, {type = 'spread', direction = 'right', padding = Auto.Default(padding, AutoUI.Pad.thin)})
	UiPush()
end

function AutoUI.SpreadLeft(padding)
	table.insert(Stack, {type = 'spread', direction = 'left', padding = Auto.Default(padding, AutoUI.Pad.thin)})
	UiPush()
end

function AutoUI.SpreadVerticle(count)
	table.insert(Stack, {type = 'spread', direction = 'verticle', length = UiHeight(), count = count})
	UiPush()
end

function AutoUI.SpreadHorizontal(count)
	table.insert(Stack, { type = 'spread', direction = 'horizontal', length = UiWidth(), count = count })
	UiPush()
end

function AutoUI.GetSpread()
	local count = Auto.Count(Stack)
	for i = count, 1, -1 do
		if Stack[i].type == 'spread' then
			return Stack[i]
		end
	end
	return nil
end

function AutoUI.SetSpread(Spread)
	local count = Auto.Count(Stack)
	for i = count, 1, -1 do
		if Stack[i].type == 'spread' then
			x = Stack[i]
		end
	end

	x = Spread
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


function HandleSpread(gs, data, type)
	if gs ~= nil then
		if gs.direction == 'down' then
			UiTranslate(0, data.rect.h + gs.padding)
		elseif gs.direction == 'up' then
			UiTranslate(0, -(data.rect.h + gs.padding))
		elseif gs.direction == 'right' then
			UiTranslate(data.rect.w + gs.padding, 0)
		elseif gs.direction == 'left' then
			UiTranslate(-(data.rect.w + gs.padding), 0)
		elseif gs.direction == 'verticle' then
			UiTranslate(0, gs.length / gs.count * 1.5 + gs.length / gs.count)
		elseif gs.direction == 'horizontal' then
			UiTranslate(gs.length / gs.count, 0)
		end
	end

	table.insert(Stack, { type = type, data = data })
end
-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface Creation Functions-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoUI.Container(width, height, alignment, padding, clip, draw)
	width = Auto.Default(width, 300)
	height = Auto.Default(height, 400)
	alignment = Auto.Default(alignment, 'left top')
	padding = math.max(Auto.Default(padding, AutoUI.Pad.micro), 0)
	clip = Auto.Default(clip, false)
	draw = Auto.Default(draw, true)

	local paddingwidth = width + padding * 2
	local paddingheight = height + padding * 2
	
	UiAlign(alignment)
	UiWindow(paddingwidth, paddingheight, false)

	UiAlign('left top')
	if draw then
		UiPush()
			UiColor(unpack(AutoUI.SecondaryColor))
			UiImageBox("ui/common/box-solid-10.png", UiWidth(), UiHeight(), 10, 10)
		UiPop()
	end

	hover = UiIsMouseInRect(UiWidth(), UiHeight())
	
	UiTranslate(padding, padding)
	UiWindow(width, height, clip)

	local offset = {x = 0, y = 0}

	UiTranslate(offset.x, offset.y)
	
	return { rect = { w = paddingwidth, h = paddingheight }, hover = hover }
end

function AutoUI.Button(name, fontsize, paddingwidth, paddingheight, draw)
	fontsize = Auto.Default(fontsize, 28)
	paddingwidth = Auto.Default(paddingwidth, AutoUI.Pad.thick)
	paddingheight = Auto.Default(paddingheight, AutoUI.Pad.thin)
	draw = Auto.Default(draw, true)

	UiPush()
		UiWordWrap(UiWidth() - AutoUI.Pad.thick)
		UiFont("regular.ttf", fontsize)
		UiButtonHoverColor(unpack(AutoUI.SpecialColor))
		UiButtonPressColor(0.75, 0.75, 0.75, 1)
		UiButtonPressDist(0.25)

		UiColor(0, 0, 0, 0)
		local rw, rh = UiText(name)
		local padrw, padrh = rw + paddingwidth * 2, rh + paddingheight * 2
		
		if draw then
			hover = UiIsMouseInRect(padrw, padrh)
			UiColor(unpack(AutoUI.PrimaryColor))
			
		UiButtonImageBox('ui/common/box-outline-6.png', 6, 6, unpack(AutoUI.PrimaryColor))
			pressed = UiTextButton(name, padrw, padrh)
		end
	UiPop()

	local data = { pressed = pressed, hover = hover, rect = { w = padrw, h = padrh } }
	if draw then HandleSpread(AutoUI.GetSpread(), data, 'draw') end

	return pressed, data
end

function AutoUI.Text(name, fontsize, paddingwidth, paddingheight, draw)
	fontsize = Auto.Default(fontsize, 28)
	paddingwidth = Auto.Default(paddingwidth, AutoUI.Pad.none)
	paddingheight = Auto.Default(paddingheight, AutoUI.Pad.none)
	draw = Auto.Default(draw, true)

	UiPush()
		UiWordWrap(UiWidth() - AutoUI.Pad.thick)
		UiFont("regular.ttf", fontsize)

		UiColor(0, 0, 0, 0)
		local rw, rh = UiText(name)
		local padrw, padrh = rw + paddingwidth, rh + paddingheight

		if draw then
			UiPush()
				UiColor(unpack(AutoUI.PrimaryColor))
				UiText(name)
			UiPop()
		end
	UiPop()

	local data = { rect = { w = padrw, h = padrh } }
	if draw then HandleSpread(AutoUI.GetSpread(), data, 'draw') end

	return data
end

function AutoUI.Slider(set, min, max, lockincrement, paddingwidth, paddingheight)
	min = Auto.Default(min, 0)
	max = Auto.Default(max, 1)
	set = Auto.Default(set, min)
	lockincrement = Auto.Default(lockincrement, 0)
	paddingwidth = Auto.Default(paddingwidth, AutoUI.Pad.thick)
	paddingheight = Auto.Default(paddingheight, AutoUI.Pad.thin)

	local width = UiWidth() - paddingwidth * 2
	local dotwidth, dotheight = UiGetImageSize("ui/common/dot.png")

	local screen = Auto.Map(set, min, max, 0, width)

	UiPush()
		UiTranslate(paddingwidth, paddingheight)
		UiColor(unpack(AutoUI.SpecialColor))
	
		UiPush()
			UiTranslate(0, dotheight / 2)
			UiRect(width, 2)
		UiPop()
			
		UiTranslate(-dotwidth / 2, 0)

		screen, released = UiSlider('ui/common/dot.png', "x", screen, 0, width)
		screen = Auto.Map(screen, 0, width, min, max)
		screen = Auto.Round(screen, lockincrement)
		screen = Auto.Clamp(screen, min, max)
		set = screen
	UiPop()

	local data = { value = set, released = released, rect = {w = width, h = paddingheight * 2} }
	HandleSpread(AutoUI.GetSpread(), data, 'draw')

	return set, data
end

function AutoUI.Image(path, width, height, alpha, draw)
	local w, h = UiGetImageSize(path)
	width = Auto.Default(width, (height == nil and UiWidth() or (height * (w / h))))
	height = Auto.Default(height, width * (h / w))
	alpha = Auto.Default(alpha, 1)
	draw = Auto.Default(draw, true)
	
	if draw then
		UiPush()
			UiColor(1, 1, 1, alpha)
			UiImageBox(path, width, height)
		UiPop()
	end

	local hover = UiIsMouseInRect(width, height)
	
	local data = {hover = hover, rect = {w = width, h = height}}
	if draw then HandleSpread(AutoUI.GetSpread(), data, 'draw') end
	
	return data
end

function AutoUI.Marker(size)
	size = Auto.Default(size, 1) / 2
	UiPush()
		UiAlign('center middle')
		UiScale(size, size)
		UiColor(unpack(AutoUI.SpecialColor))
		UiImage('ui/common/dot.png')
	UiPop()
end

function AutoUI.Tooltip(text, position, fontsize, alpha, bold)
	text = Auto.Default(text or "nil")
	fontsize = Auto.Default(fontsize or 24)
	alpha = Auto.Default(alpha or 0.75)
	bold = Auto.Default(bold or false)

	UiPush()
		UiAlign('center middle')
		local x, y = UiWorldToPixel(position)
		UiTranslate(x, y)
		UiWordWrap(UiMiddle())

		UiFont(bold and "bold.ttf" or "regular.ttf", fontsize)
		UiColor(0, 0, 0, 0)
		local rw, rh = UiText(text)

		UiColor(0, 0, 0, alpha)
		UiRect(rw, rh)

		UiColor(unpack(AutoUI.PrimaryColor))
		UiText(text)
	UiPop()
end
