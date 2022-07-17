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
function AutoLogistic(v, max, steep, offset)
	v = AutoDefault(v, math.random(0, 10000) / 10000)
	return max / (1 + math.exp((v - offset) * steep))
end

function AutoLogisticScaled(v, max, steep, offset, rangemin, rangemax)
	v = AutoLogistic(v, max, steep, offset)
	local a = AutoLogistic(rangemin, max, steep, offset)
	local b = AutoLogistic(rangemax, max, steep, offset)
	return AutoMap(v, a, b, 0, 1)
end

---'n' is the number you want rounded, 'd' is the decimal, so something like 0.1, 0.01, 1, 2, 100, 2.5
---This was a Challenge by @TallTim and @1ssnl to make the smallest rounding function, but I expanded it to make it easier to read and a little more efficent
---@param n number
---@param d number
---@return number
function AutoRound(n,d)
	d = AutoDefault(d, 1)
	if d == 0 then return n end
	v = 1 / d
	return math.floor(n * v + 0.5) / v
end

---Limits a value from going below the min and above the max
---@param val number
---@param min number
---@param max number
---@return number
function AutoClamp(val, min, max)
	min = AutoDefault(min, 0)
	max = AutoDefault(max, 1)
	if val < min then
		return min
	elseif val > max then
		return max
	else
		return val
	end
end

function AutoBiggest(...)
	local best = -math.huge
	for i in pairs(arg) do
		if i > best then best = i end
	end
	return best
end

function AutoSmallest(...)
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
function AutoRndVec(length, precision)
	precision = AutoDefault(precision, 0.01)
	local m = 1/precision
	local v = VecNormalize(Vec(math.random(-m,m), math.random(-m,m), math.random(-m,m)))
	return VecScale(v, length)	
end

---Lerp function, Is not clamped meaning it if t is above 1 then it will 'overshoot'
---@param a number
---@param b number
---@param t number
---@return number
function AutoLerpUnclamped(a, b, t)
	return (1-t)*a + t*b
end

--- Moves a towards b by t
---@param a number
---@param b number
---@param t number
---@return number
function AutoMove(a, b, t)
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

function AutoMap(x, a1, a2, b1, b2)
	if a1 == a2 then return b2 end
	return b1 + ((x - a1) * (b2 - b1)) / (a2 - a1)
end

---Return the Distance between Two Vectors
---@param a Vec
---@param b Vec
---@return number
function AutoVecDist(a, b)
	return math.sqrt( (a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2 )
end

function AutoVecRescale(a, b)
	return VecScale(VecNormalize(a), b)
end

function AutoVecClamp(v, min, max)
	local l = VecLength(v)
	if l > max then
		return AutoVecRescale(v, max)
	elseif l < min then
		return AutoVecRescale(v, min)
	else
		return v
	end
end

function AutoVecSubsituteY(v, y)
	return Vec(v[1], y, v[3])
end

---Return the Distance between the numbers a and b
---@param a number
---@param b number
---@return number
function AutoDist(a, b)
	return math.abs(a - b)
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Utility-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Counts the amount of elements in a list
---@param t table
---@return integer
function AutoCount(t)
	local c = 0
	for i in pairs(t) do
		c = c + 1
	end

	return c
end

function AutoLast(t)
	return t[AutoCount(t)]
end

function AutoPack(...)
	-- Returns a new table with parameters stored into an array, with field "n" being the total number of parameters
	local t = { ... }
	t.n = #t
	return t
end

function AutoDefault(val, default)
	if val == nil then return default else return val end
end

---A workaround to making a table readonly
---@param tbl table
---@return table
function AutoSetReadOnly(tbl)
	return setmetatable({}, {
		__index = tbl,
		__newindex = function(t, key, value)
			error("attempting to change constant " .. tostring(key) .. " to " .. tostring(value), 2)
		end
	})
end

-- Code donatated from @Dr. HypnoTox - Thankyou!
function AutoToString(object)
	if object == nil then
		return 'nil'
	end

	if (type(object) == 'number') or (type(object) == 'string') or (type(object) == 'boolean') then
		return tostring(object)
	end

	local toDump = '{'

	for k, v in pairs(object) do
		if (type(k) == 'number') then
			toDump = toDump .. '[' .. k .. '] = '
		elseif (type(k) == 'string') then
			toDump = toDump .. k .. '= '
		end

		if (type(v) == 'number') then
			toDump = toDump .. v .. ','
		elseif (type(v) == 'string') then
			toDump = toDump .. '\'' .. v .. '\', '
		else
			toDump = toDump .. AutoToString(v) .. ', '
		end
	end

	toDump = toDump .. '}'

	return toDump
end

function AutoPrint(...)
	arg.n = nil
	DebugPrint(AutoToString(arg))
	return unpack(arg)
end

function AutoClearConsole()
	for i = 1, 24 do DebugPrint('') end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Game-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoDrawOutlineBlink(entity, speed, time, red, green, blue, alphamulti)
	speed = AutoDefault(speed, 1)
	time = AutoDefault(time, GetTime)()
	red = AutoDefault(red, 1)
	green = AutoDefault(green, 1)
	blue = AutoDefault(blue, 1)
	alphamulti = AutoDefault(alphamulti, 1)

	local t = (time * speed) % 2
	local alpha = AutoLogistic(t, 1, -15, 0.4) * AutoLogistic(t, 1, 15, 1.4)

	local type = GetEntityType(entity)
	if type == 'body' then
		DrawBodyOutline(entity, red, green, blue, alpha * alphamulti)
	elseif type == 'shape' then
		DrawShapeOutline(entity, red, green, blue, alpha * alphamulti)
	end
end

function AutoRetrievePath(precision)
	precision = AutoDefault(precision, 0.2)

	local path = {}
	local length = GetPathLength()
	local l = 0
	while l < length do
		path[#path + 1] = GetPathPoint(l)
		l = l + precision
	end

	return path, path[#path]
end

function AutoQueryRejectBodies(bodies)
	for i in pairs(bodies) do
		QueryRejectBody(bodies[i])
	end
end

function AutoSetBodyCollisionFilter(body, layer, masknummber)
	local shapes = GetBodyShapes(body)
	for i in pairs(shapes) do
		SetShapeCollisionFilter(shapes[i], layer, masknummber)
	end
end

function AutoWorldCenterOfMass(body)
	local trans = GetBodyTransform(body)
	local pos = TransformToParentPoint(trans, GetBodyCenterOfMass(body))
	return pos
end

function AutoSpeed(body)
	return VecLength(GetBodyVelocity(body))
end

function AutoPredictPosition(body, time)
	local pos = AutoWorldCenterOfMass(body)
	local vel = GetBodyVelocity(body)
	local newpos = VecCopy(pos)

	for steps = 0, time, GetTimeStep() do
		vel = VecAdd(vel, VecScale(Vec(0, -10, 0), GetTimeStep()))
		newpos = VecAdd(newpos, VecScale(vel, GetTimeStep()))
		-- newpos = VecAdd(newpos, VecScale(Vec(0, -10, 0), GetTimeStep()))
	end

	local dir = VecNormalize(VecSub(newpos, pos))

	return newpos, vel, dir
end

function AutoDrawPath(lines)
	for i = 1, #lines - 1 do
		local color = (lines[i].color + lines[i + 1].color) / 2
		DebugLine(lines[i].pos, lines[i + 1].pos, color, 1 - color, 0.5 - color)
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Registry-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoKeyDefaultInt(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetInt(path)
	else
		SetInt(path, default)
		return default
	end
end

function AutoKeyDefaultFloat(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetFloat(path)
	else
		SetFloat(path, default)
		return default
	end
end

function AutoKeyDefaultString(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetString(path)
	else
		SetString(path, default)
		return default
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

AutoUIPad = {none = 0, atom = 4, micro = 6, thin = 12, thick = 24, heavy = 48, beefy = 128}
setmetatable(AutoUIPad, { __call = function (t, padding) UiTranslate(padding, padding) end})

AutoUIPrimaryColor = {0.95, 0.95, 0.95, 1}
AutoUISpecialColor = {1, 1, 0.55, 1}
AutoUISecondaryColor = {0, 0, 0, 0.55}
local Stack = {}


function AutoUIAlignmentToPos(alignment)
	v, y = 0, 0
	if string.find(alignment, 'left') then v = -1 end
	if string.find(alignment, 'center') then v = 0 end
	if string.find(alignment, 'right') then v = 1 end
	if string.find(alignment, 'bottom') then y = -1 end
	if string.find(alignment, 'middle') then y = 0 end
	if string.find(alignment, 'top') then y = 1 end
	return {x = v, y = y}
end

function AutoUICenter()
	UiTranslate(UiCenter(), UiMiddle())
	UiAlign('center middle')
end

function AutoUISpreadDown(padding)
	table.insert(Stack, {type = 'spread', direction = 'down', padding = AutoDefault(padding, AutoUIPad.thin)})
	UiPush()
end

function AutoUISpreadUp(padding)
	table.insert(Stack, {type = 'spread', direction = 'up', padding = AutoDefault(padding, AutoUIPad.thin)})
	UiPush()
end

function AutoUISpreadRight(padding)
	table.insert(Stack, {type = 'spread', direction = 'right', padding = AutoDefault(padding, AutoUIPad.thin)})
	UiPush()
end

function AutoUISpreadLeft(padding)
	table.insert(Stack, {type = 'spread', direction = 'left', padding = AutoDefault(padding, AutoUIPad.thin)})
	UiPush()
end

function AutoUISpreadVerticle(count)
	table.insert(Stack, {type = 'spread', direction = 'verticle', length = UiHeight(), count = count})
	UiPush()
end

function AutoUISpreadHorizontal(count)
	table.insert(Stack, { type = 'spread', direction = 'horizontal', length = UiWidth(), count = count })
	UiPush()
end

function AutoUIGetSpread(l)
	l = AutoDefault(l, 1)
	_l = 0
	local count = AutoCount(Stack)
	for i = count, 1, -1 do
		if Stack[i].type == 'spread' then
			_l = _l + 1
			if _l >= 1 then
				return Stack[i]
			end
		end
	end
	return nil
end

function AutoUISetSpread(Spread)
	local count = AutoCount(Stack)
	for i = count, 1, -1 do
		if Stack[i].type == 'spread' then
			v = Stack[i]
		end
	end

	v = Spread
end

function AutoUISpreadEnd()
	local unitdata = {comb = { w = 0, h = 0 }, max = { w = 0, h = 0 }}
	local Spread = AutoUIGetSpread()
	local LastSpread = AutoUIGetSpread(2)
	
	while true do
		local count = AutoCount(Stack)
		
		if Stack[count].type ~= 'spread' then
			if Stack[count].data.rect then
				local rect = Stack[count].data.rect
				unitdata.comb.w, unitdata.comb.h = unitdata.comb.w + rect.w, unitdata.comb.h + rect.h
				unitdata.max.w, unitdata.max.h = math.max(unitdata.max.w, rect.w), math.max(unitdata.max.h, rect.h)
			end

			table.remove(Stack, count)
		else
			UiPop()
			table.remove(Stack, count)

			return unitdata
		end
		if count <= 1 then return unitdata end
	end
end


function HandleSpread(gs, data, type, spreadpad)
	spreadpad = AutoDefault(spreadpad, false)

	if gs ~= nil then
		if not spreadpad then pad = 0 else pad = gs.padding end
		if gs.direction == 'down' then
			UiTranslate(0, data.rect.h + pad)
		elseif gs.direction == 'up' then
			UiTranslate(0, -(data.rect.h + pad))
		elseif gs.direction == 'right' then
			UiTranslate(data.rect.w + pad, 0)
		elseif gs.direction == 'left' then
			UiTranslate(-(data.rect.w + pad), 0)
		elseif gs.direction == 'verticle' then
			UiTranslate(0, gs.length / gs.count * 1.5 + gs.length / gs.count)
		elseif gs.direction == 'horizontal' then
			UiTranslate(gs.length / gs.count, 0)
		end
	end

	if type ~= nil then
		table.insert(Stack, { type = type, data = data })
	end
end
-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface Creation Functions-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoUIContainer(width, height, alignment, padding, clip, draw, spreadpad)
	width = AutoDefault(width, 300)
	height = AutoDefault(height, 400)
	alignment = AutoDefault(alignment, 'left top')
	padding = math.max(AutoDefault(padding, AutoUIPad.micro), 0)
	clip = AutoDefault(clip, false)
	draw = AutoDefault(draw, true)
	spreadpad = AutoDefault(spreadpad, true)

	local paddingwidth = math.max(width - padding * 2, padding * 2)
	local paddingheight = math.max(height - padding * 2, padding * 2)
	
	UiAlign(alignment)
	UiWindow(width, height, false)

	UiAlign('left top')
	if draw then
		UiPush()
			UiColor(unpack(AutoUISecondaryColor))
			UiImageBox("ui/common/box-solid-10.png", UiWidth(), UiHeight(), 10, 10)
		UiPop()
	end

	hover = UiIsMouseInRect(UiWidth(), UiHeight())
	
	UiTranslate(padding, padding)
	UiWindow(paddingwidth, paddingheight, clip)

	local offset = {x = 0, y = 0}

	UiTranslate(offset.x, offset.y)
	
	return { rect = { w = paddingwidth, h = paddingheight }, hover = hover }
end

function AutoUIButton(name, fontsize, paddingwidth, paddingheight, draw, spreadpad)
	fontsize = AutoDefault(fontsize, 28)
	paddingwidth = AutoDefault(paddingwidth, AutoUIPad.thick)
	paddingheight = AutoDefault(paddingheight, AutoUIPad.thin)
	draw = AutoDefault(draw, true)
	spreadpad = AutoDefault(spreadpad, true)

	UiPush()
		UiWordWrap(UiWidth() - AutoUIPad.thick)
		UiFont("regular.ttf", fontsize)
		UiButtonHoverColor(unpack(AutoUISpecialColor))
		UiButtonPressColor(0.75, 0.75, 0.75, 1)
		UiButtonPressDist(0.25)

		UiColor(0, 0, 0, 0)
		local rw, rh = UiText(name)
		local padrw, padrh = rw + paddingwidth * 2, rh + paddingheight * 2
		
		if draw then
			hover = UiIsMouseInRect(padrw, padrh)
			UiColor(unpack(AutoUIPrimaryColor))
			
		UiButtonImageBox('ui/common/box-outline-6.png', 6, 6, unpack(AutoUIPrimaryColor))
			pressed = UiTextButton(name, padrw, padrh)
		end
	UiPop()

	local data = { pressed = pressed, hover = hover, rect = { w = padrw, h = padrh } }
	if draw then HandleSpread(AutoUIGetSpread(), data, 'draw', spreadpad) end

	return pressed, data
end

function AutoUIText(name, fontsize, draw, spreadpad)
	fontsize = AutoDefault(fontsize, 28)
	paddingwidth = AutoDefault(paddingwidth, AutoUIPad.none)
	paddingheight = AutoDefault(paddingheight, AutoUIPad.none)
	draw = AutoDefault(draw, true)
	spreadpad = AutoDefault(spreadpad, true)

	UiPush()
		UiWordWrap(UiWidth() - AutoUIPad.thick)
		UiFont("regular.ttf", fontsize)

		UiColor(0, 0, 0, 0)
		local rw, rh = UiText(name)

		if draw then
			UiPush()
				UiWindow(rw + paddingwidth * 2, rh + paddingheight * 2)
				AutoUICenter()
				
				UiColor(unpack(AutoUIPrimaryColor))
				UiText(name)
			UiPop()
		end
	UiPop()

	local data = { rect = { w = rw + paddingwidth * 2, h = rh + paddingheight * 2} }
	if draw then HandleSpread(AutoUIGetSpread(), data, 'draw', spreadpad) end

	return data
end

function AutoUISlider(set, min, max, lockincrement, paddingwidth, paddingheight, spreadpad)
	min = AutoDefault(min, 0)
	max = AutoDefault(max, 1)
	set = AutoDefault(set, min)
	lockincrement = AutoDefault(lockincrement, 0)
	paddingwidth = AutoDefault(paddingwidth, AutoUIPad.thick)
	paddingheight = AutoDefault(paddingheight, AutoUIPad.micro)
	spreadpad = AutoDefault(spreadpad, true)

	local width = UiWidth() - paddingwidth * 2
	local dotwidth, dotheight = UiGetImageSize("ui/common/dot.png")

	local screen = AutoMap(set, min, max, 0, width)

	UiPush()
		UiTranslate(paddingwidth, paddingheight)
		UiColor(unpack(AutoUISpecialColor))
	
		UiPush()
			UiTranslate(0, dotheight / 2)
			UiRect(width, 2)
		UiPop()
			
		UiTranslate(-dotwidth / 2, 0)

		screen, released = UiSlider('ui/common/dot.png', "x", screen, 0, width)
		screen = AutoMap(screen, 0, width, min, max)
		screen = AutoRound(screen, lockincrement)
		screen = AutoClamp(screen, min, max)
		set = screen
	UiPop()

	local data = { value = set, released = released, rect = {w = width, h = paddingheight * 2 + dotheight} }
	HandleSpread(AutoUIGetSpread(), data, 'draw', spreadpad)
	
	return set, data
end

function AutoUIImage(path, width, height, alpha, draw, spreadpad)
	local w, h = UiGetImageSize(path)
	width = AutoDefault(width, (height == nil and UiWidth() or (height * (w / h))))
	height = AutoDefault(height, width * (h / w))
	alpha = AutoDefault(alpha, 1)
	draw = AutoDefault(draw, true)
	spreadpad = AutoDefault(spreadpad, true)
	
	if draw then
		UiPush()
			UiColor(1, 1, 1, alpha)
			UiImageBox(path, width, height)
		UiPop()
	end

	local hover = UiIsMouseInRect(width, height)
	
	local data = {hover = hover, rect = {w = width, h = height}}
	if draw then HandleSpread(AutoUIGetSpread(), data, 'draw', spreadpad) end
	
	return data
end

function AutoUIMarker(size)
	size = AutoDefault(size, 1) / 2
	UiPush()
		UiAlign('center middle')
		UiScale(size, size)
		UiColor(unpack(AutoUISpecialColor))
		UiImage('ui/common/dot.png')
	UiPop()
end

function AutoUITooltip(text, position, fontsize, alpha, bold)
	text = AutoDefault(text or "nil")
	fontsize = AutoDefault(fontsize or 24)
	alpha = AutoDefault(alpha or 0.75)
	bold = AutoDefault(bold or false)

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

		UiColor(unpack(AutoUIPrimaryColor))
		UiText(text)
	UiPop()
end
