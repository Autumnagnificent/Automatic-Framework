-- VERSION 2.95
-- I ask that you please do not rename Automatic.lua - Thankyou

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Documentation--------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---@class plane: { pos:vector, rot:quaternion, size:{ [1]:number, [2]:number } }
---@class OBB: { pos:vector, rot:quaternion, size:vector }

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Shortcuts------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

AutoFlatSprite = LoadSprite('ui/menu/white_32.png')
AutoColors = {
	background_dark = { 0.28627450980392, 0.25490196078431, 0.38039215686275 },
	background_light = { 0.41960784313725, 0.39607843137255, 0.58823529411765 },
	wood_dark = { 0.6, 0.33725490196078, 0.42352941176471 },
	wood_light = { 0.78039215686275, 0.53333333333333, 0.56470588235294 },
	rock_dark = { 0.41960784313725, 0.38039215686275, 0.46666666666667 },
	rock_light = { 0.49803921568627, 0.46274509803922, 0.55686274509804 },
	green_dark = { 0.3843137254902, 0.76078431372549, 0.76078431372549 },
	green_light = { 0.4156862745098, 0.90980392156863, 0.63529411764706 },
	jade_dark = { 0.33725490196078, 0.52156862745098, 0.6 },
	jade_light = { 0.29411764705882, 0.68627450980392, 0.69019607843137 },
	aqua_dark = { 0.28627450980392, 0.46666666666667, 0.58039215686275 },
	aqua_light = { 0.32156862745098, 0.60392156862745, 0.78039215686275 },
	pastel_dark = { 1, 0.7921568627451, 0.83137254901961 },
	pastel_light = { 0.80392156862745, 0.70588235294118, 0.85882352941176 },
	pink_dark = { 0.70196078431373, 0.45098039215686, 0.64313725490196 },
	pink_light = { 0.94901960784314, 0.57647058823529, 0.86274509803922 },
	purple_dark = { 0.56470588235294, 0.34117647058824, 0.63921568627451 },
	purple_light = { 0.77647058823529, 0.45098039215686, 0.8156862745098 },
	yellow_dark = { 0.7921568627451, 0.65490196078431, 0.32156862745098 },
	yellow_light = { 0.89803921568627, 0.75686274509804, 0.36862745098039 },
	amber_dark = { 0.7843137254902, 0.50196078431373, 0.28627450980392 },
	amber_light = { 0.96470588235294, 0.63921568627451, 0.18039215686275 },
	red_dark = { 0.72549019607843, 0.35686274509804, 0.48627450980392 },
	red_light = { 0.84313725490196, 0.33333333333333, 0.41960784313725 },
	white_dark = { 0.84705882352941, 0.74509803921569, 0.61960784313725 },
	white_light = { 0.96470588235294, 0.91372549019608, 0.80392156862745 },
	blue_dark = { 0.2078431372549, 0.31372549019608, 0.43921568627451 },
	blue_light = { 0.19607843137255, 0.61176470588235, 0.78823529411765 },
	alert_dark = { 0.22352941176471, 0.098039215686275, 0.2 },
	alert_light = { 0.74901960784314, 0.21960784313725, 0.49019607843137 },
}

---comment
---@param baseline string
---@return { C:number, Cs:number, D:number, Ds:number, E:number, F:number, Fs:number, G:number, Gs:number, A:number, As:number, B:number }
function AutoNoteFrequency(baseline)
	local f = {
		C  = 261.63,
		Cs = 277.18,
		D  = 293.66,
		Ds = 311.13,
		E  = 329.63,
		F  = 349.23,
		Fs = 369.99,
		G  = 392.00,
		Gs = 415.30,
		A  = 440.00,
		As = 466.16,
		B  = 493.88,
	}

	local tuned = {}
	for note, freq in pairs(f) do
		tuned[note] = freq / f[baseline]
	end

	return tuned
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Arithmetic Functions-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Logistic function, Can be used for juicy UI and smooth easing among other things.
---https://www.desmos.com/calculator/cmmwrjtyit?invertedColors
---@param v number|nil Input number, if nil then it will be a Random number between 0 and 1
---@param max number The Maximum value
---@param steep number How steep the curve is
---@param offset number The horizontal offset of the middle of the curve
---@return number
function AutoLogistic(v, max, steep, offset)
	v = AutoDefault(v, math.random(0, 10000) / 10000)
	return (max or 1) / (1 + math.exp((v - (offset or 0.5)) * (steep or -10)))
end

---Logistic function followed by a mapping function, guarantees that the return value will be between 0 and 1
---@param v number|nil Random number between 0 and 1
---@param max number The Maximum value
---@param steep number How steep the curve is
---@param offset number The horizontal offset of the middle of the curve
---@param rangemin number Maps this number to 0
---@param rangemax number Maps this number to 1
---@return number
function AutoLogisticScaled(v, max, steep, offset, rangemin, rangemax)
	v = AutoLogistic(v, max, steep, offset)
	local a = AutoLogistic(rangemin, max, steep, offset)
	local b = AutoLogistic(rangemax, max, steep, offset)
	local mapped = AutoMap(v, a, b, 0, 1)
	return AutoClamp(mapped, 0, 1)
end

---This was a Challenge by @TallTim and @1ssnl to make the smallest rounding function, but I expanded it to make it easier to read and a little more efficent
---@param v number Input number
---@param increment number|nil The lowest increment. A Step of 1 will round the number to 1, A step of 5 will round it to the closest increment of 5, A step of 0.1 will round to the tenth. Default is 1
---@return number
function AutoRound(v, increment)
	increment = AutoDefault(increment, 1)
	if increment == 0 then return v end
	local s = 1 / increment
	return math.floor(v * s + 0.5) / s
end

---Maps a value from range a1-a2 to range b1-b2
---@param v number Input number
---@param a1 number Goes from the range of number a1
---@param a2 number To number a2
---@param b1 number To the range of b1
---@param b2 number To number b2
---@param clamp boolean|nil Clamp the number between b1 and b2, Default is false
---@return number
function AutoMap(v, a1, a2, b1, b2, clamp)
	clamp = AutoDefault(clamp, false)
	if a1 == a2 then return b2 end
	local mapped = b1 + ((v - a1) * (b2 - b1)) / (a2 - a1)
	return clamp and AutoClamp(mapped, math.min(b1, b2), math.max(b1, b2)) or mapped
end

---Limits a value from going below the min and above the max
---@param v number The number to clamp
---@param min number|nil The minimum the number can be, Default is 0
---@param max number|nil The maximum the number can be, Default is 1
---@return number
function AutoClamp(v, min, max)
	min = AutoDefault(min, 0)
	max = AutoDefault(max, 1)
	return math.max(math.min(v, max), min)
end

---Limits a value from going below the min and above the max
---@param v number The number to clamp
---@param max number|nil The maximum the length of the number can be, Default is 1
---@return number
function AutoClampLength(v, max)
	max = AutoDefault(max, 1)
	if v < -max then
		return -max
	elseif v > max then
		return max
	else
		return v
	end
end

---Wraps a value inbetween a range, Thank you iaobardar for the Optimization
---@param v number The number to wrap
---@param min number|nil The minimum range
---@param max number|nil The maximum range
---@return number
function AutoWrap(v, min, max)
	min = AutoDefault(min, 0)
	max = AutoDefault(max, 1)

	return (v - min) % ((max + 1) - min) + min
end

---Lerp function, If t is above 1 then it will 'overshoot'
---@param a number Goes from number A
---@param b number To number B
---@param t number Interpolated by T
---@return number
function AutoLerp(a, b, t)
	return (1 - t) * a + t * b
end

function AutoLerpWrap(a, b, t, w)
	local m = w
	local da = (b - a) % m
	local n = (da * 2) % m - da
	return a + n * t
end

---Moves a towards b by t
---@param a number Goes from number A
---@param b number To number B
---@param t number Moved by T
---@return number
function AutoMove(a, b, t)
	output = a
	if a == b then
		return a
	elseif a > b then
		output = math.max(a - t, b)
	else
		output = math.min(a + t, b)
	end

	return output
end

---Return the Distance between the numbers a and b
---@param a number
---@param b number
---@return number
function AutoDist(a, b)
	return math.abs(a - b)
end

---Normalizes all values in a table to have a magnitude of 1 - Scales every number to still represent the same "direction"
---@param t table { 1, 2, 3, 4 }
---@param scale number Defualt 1
---@return table
function AutoNormalize(t, scale)
	local norm = {}
	local maxabs = 0
	for i = 1, #t do
		local abs = math.abs(t[i])
		maxabs = abs > maxabs and abs or maxabs
	end

	for i = 1, #t do
		norm[i] = t[i] / maxabs * AutoDefault(scale, 1)
	end
	return norm
end

---Takes a table of weights, like {1, 2, 0.5, 0.5}, and produces a table of how much space each weight would take up if it were to span over a given length.
---If given the weights {1, 2, 0.5, 0.5}, with a span length of 100, the resulting table would be = {25, 50, 12.5, 12.5}.
---A padding parameter can also be added which can be used to make Ui easier. Iterate through the resulting table, after each UiRect, move the width + the padding parameter
---@param weights table|number weights
---@param span number
---@param padding number
---@return table
function AutoFlex(weights, span, padding)
	local istable = type(weights) == "table"
	weights = not istable and (function()
		local t = {}
		for i = 1, weights do
			t[i] = 1
		end
		return t
	end)() or weights

	span = AutoDefault(span, 1)
	padding = AutoDefault(padding, 0)
	local count = #weights

	local flexxed = {}
	local normalized = AutoNormalize(weights)
	local max = 0
	for i = 1, count do
		max = max + normalized[i]
	end


	local padding_deduction = (padding / count) * (count - 1)
	for i = 1, count do
		flexxed[i] = (normalized[i] / max) * span - padding_deduction
	end
	return flexxed
end

---Takes in a variable number of arguments which are considered as weights. It returns a number, the index of the selected weight using a bias based on the weight values. Good for Biased Randomness.
---@param ... weights
---@return number selected
function AutoBias(...)
	local T = {}
	local max = 0
	for i = 1, #arg do
		max = max + arg[i]
		T[i] = {}
		T[i].i = i
		T[i].w = arg[i]
	end

	table.sort(T, function(a, b)
		return a.w < b.w
	end)

	local r = math.random() * max
	local cursor = 0
	for i = 1, #T do
		cursor = cursor + T[i].w
		if r <= cursor then
			return T[i].i
		end
	end
end

---Rebuilds a table in a given order
---@param vec any
---@param swizzle any
---@return table
function AutoSwizzle(vec, swizzle)
	local swizzleMap = { x = 1, y = 2, z = 3, w = 4, r = 1, g = 2, b = 3, a = 4 }
	local built = {}
	for i = 1, #swizzle do
        local axis = swizzle:sub(i, i)
		local asnum = tonumber(axis)
		built[i] = vec[asnum or swizzleMap[axis]]
	end
	return built
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Vector Functions-----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoVecEquals(a, b)
    for i, va in pairs(a) do
		if va ~= b[i] then return false end
    end

	return true
end

--- Return a Random Vector with an optional offset and scale
---@param param1 number|vector
---@param param2 number|nil
---@return vector
function AutoVecRnd(param1, param2)
	local offset, scale
	if type(param1) == "table" then
		offset = param1
		scale = param2 or 1
	else
		offset = { 0, 0, 0 }
		scale = param1
	end

	local rndVec = VecNormalize({
		(math.random() * 2 - 1),
		(math.random() * 2 - 1),
		(math.random() * 2 - 1),
	})

	local v = VecAdd(offset, VecScale(rndVec, scale))
	return v
end

---Return the Distance between Two Vectors
---@param a vector
---@param b vector
---@return number
function AutoVecDist(a, b)
	return VecLength(VecSub(b, a))
end

function AutoVecMove(vec, dir, dist)
	return VecAdd(vec, VecScale(dir, dist))
end

---Return the Vector Rounded to a number
---@param vec vector
---@param r number
---@return vector
function AutoVecRound(vec, r)
	return Vec(AutoRound(vec[1], r), AutoRound(vec[2], r), AutoRound(vec[3], r))
end

---Return a vector that has the magnitude of b, but with the direction of a
---@param a vector
---@param b number
---@return vector
function AutoVecRescale(a, b)
	return VecScale(VecNormalize(a), b)
end

---Maps a Vector from range a1-a2 to range b1-b2
---@param v vector Input Vector
---@param a1 number Goes from the range of number a1
---@param a2 number To number a2
---@param b1 number To the range of b1
---@param b2 number To number b2
---@return vector
function AutoVecMap(v, a1, a2, b1, b2)
	if a1 == a2 then return AutoVecRescale(v, b2) end
	local out = {
		AutoMap(v[1], a1, a2, b1, b2),
		AutoMap(v[2], a1, a2, b1, b2),
		AutoMap(v[3], a1, a2, b1, b2),
	}
	return out
end

---Limits the magnitude of a vector to be between min and max
---@param v vector The Vector to clamp
---@param min number|nil The minimum the magnitude can be, Default is 0
---@param max number|nil The maximum the magnitude can be, Default is 1
---@return vector
function AutoVecClampMagnitude(v, min, max)
	min, max = AutoDefault(min, 0), AutoDefault(max, 1)
	local l = VecLength(v)
	if l > max then
		return AutoVecRescale(v, max)
	elseif l < min then
		return AutoVecRescale(v, min)
	else
		return v
	end
end

---Limits a vector to be between min and max
---@param v vector The Vector to clamp
---@param min number|nil The minimum, Default is 0
---@param max number|nil The maximum, Default is 1
---@return vector
function AutoVecClamp(v, min, max)
	min, max = AutoDefault(min, 0), AutoDefault(max, 1)
	return {
		AutoClamp(v[1], min, max),
		AutoClamp(v[2], min, max),
		AutoClamp(v[3], min, max)
	}
end

---Return Vec(1, 1, 1) scaled by length
---@param length number return the vector of size length, Default is 1
---@return vector
function AutoVecOne(length)
	return VecScale(Vec(1, 1, 1), length or 1)
end

function AutoVecMidpoint(a, b)
	return VecScale(VecAdd(a, b), 0.5)
end

---Return Vec a multiplied by Vec b
---@param a vector
---@param b vector
---@return vector
function AutoVecMulti(a, b)
	return {
		a[1] * b[1],
		a[2] * b[2],
		a[3] * b[3],
	}
end

---Return Vec a divided by Vec b
---@param a vector
---@param b vector
---@return vector
function AutoVecDiv(a, b)
	return {
		a[1] / b[1],
		a[2] / b[2],
		a[3] / b[3],
	}
end

---Return Vec a to the Power of b
---@param a vector
---@param b number
---@return vector
function AutoVecPow(a, b)
	return {
		a[1] ^ b,
		a[2] ^ b,
		a[3] ^ b,
	}
end

---Return Vec a to the Power of Vec b
---@param a vector
---@param b vector
---@return vector
function AutoVecPowVec(a, b)
	return {
		a[1] ^ b[1],
		a[2] ^ b[2],
		a[3] ^ b[3],
	}
end

---Return Vec Absolute Value
---@param v vector
---@return vector
function AutoVecAbs(v)
	return {
		math.abs(v[1]),
		math.abs(v[2]),
		math.abs(v[3]),
	}
end

---Equivalent to math.min(unpack(v))
---@param v vector
---@return number
function AutoVecMin(v)
	return math.min(unpack(v))
end

---Equivalent to math.max(unpack(v))
---@param v vector
---@return number
function AutoVecMax(v)
	return math.max(unpack(v))
end

function AutoVecOrthoDirection(vec, scale)
	local maxValue = math.max(math.abs(vec[1]), math.abs(vec[2]), math.abs(vec[3]))

	if maxValue == math.abs(vec[1]) then
		return vec[1] > 0 and { (scale or 1), 0, 0 } or { -(scale or 1), 0, 0 }
	elseif maxValue == math.abs(vec[2]) then
		return vec[2] > 0 and { 0, (scale or 1), 0 } or { 0, -(scale or 1), 0 }
	else
		return vec[3] > 0 and { 0, 0, (scale or 1) } or { 0, 0, -(scale or 1) }
	end
end

--- Rotates a vector around an axis by a given angle
--- @param vec vector The vector to rotate
--- @param axis vector The rotation axis, a unit vector
--- @param angle number The rotation angle in degrees
--- @return vector vec The rotated vector
function AutoVecRotate(vec, axis, angle)
	local quat = QuatAxisAngle(axis, angle)
	return QuatRotateVec(quat, vec)
end

---Return Vec v with it's x value replaced by subx
---@param v vector
---@param subx number
function AutoVecSubsituteX(v, subx)
	local new = VecCopy(v)
	new[1] = subx
	return new
end

---Return Vec v with it's y value replaced by suby
---@param v vector
---@param suby number
function AutoVecSubsituteY(v, suby)
	local new = VecCopy(v)
	new[2] = suby
	return new
end

---Return Vec v with it's z value replaced by subz
---@param v vector
---@param subz number
function AutoVecSubsituteZ(v, subz)
	local new = VecCopy(v)
	new[3] = subz
	return new
end

---Converts the output of VecDot with normalized vectors to an angle
---@param dot number
---@return number
function AutoDotToAngle(dot)
	return math.deg(math.acos(dot))
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Quat Functions-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---@param rot quaternion
---@return vector
function AutoQuatFwd(rot)
	return QuatRotateVec(rot, Vec(0, 0, 1))
end

---Returns a random quaternion
---@param angle number degrees
---@return quaternion
function AutoRandomQuat(angle)
	local axis = { math.random() - 0.5, math.random() - 0.5, math.random() - 0.5 }
	local sinHalfAngle = math.sin(math.rad(angle) / 2)
	local cosHalfAngle = math.cos(math.rad(angle) / 2)
	return Quat(
		axis[1] * sinHalfAngle,
		axis[2] * sinHalfAngle,
		axis[3] * sinHalfAngle,
		cosHalfAngle
	)
end

-- Computes the dot product of two quaternions.
---@param a quaternion
---@param b quaternion
---@return number
function AutoQuatDot(a, b)
	return a[1] * b[1] + a[2] * b[2] + a[3] * b[3] + a[4] * b[4]
end

--- Returns the Conjugate of the given quaternion.
---@param quat quaternion
---@return quaternion quat
function AutoQuatConjugate(quat)
    return { -quat[1], -quat[2], -quat[3], quat[4] }
end

--- Returns the Inverse of the given quaternion.
---@param quat quaternion
---@return quaternion quat
function AutoQuatInverse(quat)
	local norm = quat[1] ^ 2 + quat[2] ^ 2 + quat[3] ^ 2 + quat[4] ^ 2
	local inverse = { -quat[1] / norm, -quat[2] / norm, -quat[3] / norm, quat[4] / norm }
	return inverse
end

--- Between -a and a, picks the quaternion nearest to b
---@param a quaternion
---@param b quaternion
---@return quaternion
---
--- Thankyou to Mathias for this function
function AutoQuatNearest(a, b)
	return AutoQuatDot(a, b) < 0 and { -a[1], -a[2], -a[3], -a[4] } or { a[1], a[2], a[3], a[4] }
end

--- Same as QuatAxisAngle() but takes a single vector instead of a unit vector + an angle, for convenience
--- Thankyou to Mathias for this function
function AutoQuatFromAxisAngle(v)
	local xyz = VecScale(v, 0.5)
	local angle = VecLength(xyz)

	if angle == 0 then
		return Quat()
	end

	local co = math.cos(angle)
	local si = math.sin(angle)
	local qXYZ = VecScale(xyz, si / angle)
	return Quat(qXYZ[1], qXYZ[2], qXYZ[3], co)
end

--- Converts a quaternion to an axis angle representation
--- Returns a rotation vector where axis is the direction and angle is the length
---
--- Thankyou to Mathias for this function
function AutoQuatToAxisAngle(q)
	local qXYZ = Vec(q[1], q[2], q[3])
	local co = q[4]
	local si = VecLength(qXYZ)

	if si == 0 then
		return VecScale(qXYZ, 2.0 / co)
	end

	local angle = math.atan2(si, co)
	return VecScale(qXYZ, 2.0 * angle / si)
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------AABB Bounds Functions-----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Get the center of a body's bounds
---@param body body_handle
---@return vector
function AutoBodyBoundsCenter(body)
	local aa, bb = GetBodyBounds(body)
	return VecScale(VecAdd(aa, bb), 0.5)
end

---Get the center of a shapes's bounds
---@param shape shape_handle
---@return vector
function AutoShapeBoundsCenter(shape)
	local aa, bb = GetShapeBounds(shape)
	return VecScale(VecAdd(aa, bb), 0.5)
end

---Returns a Axis ALigned Bounding Box with the center of pos
---@param pos vector
---@param halfextents vector|number
---@return vector lower-bound
---@return vector upper-bound
function AutoAABBExpandPoint(pos, halfextents)
	if type(halfextents) == "number" then
		halfextents = AutoVecOne(halfextents)
	end

	return VecSub(pos, halfextents), VecAdd(pos, halfextents)
end

---Takes two vectors and modifys them so they can be used in other bound functions
---@param aa vector
---@param bb vector
---@return vector
---@return vector
function AutoAABBCorrection(aa, bb)
	local min, max = VecCopy(aa), VecCopy(bb)

	if bb[1] < aa[1] then
		min[1] = bb[1]
		max[1] = aa[1]
	end
	if bb[2] < aa[2] then
		min[2] = bb[2]
		max[2] = aa[2]
	end
	if bb[3] < aa[3] then
		min[3] = bb[3]
		max[3] = aa[3]
	end

	return min, max
end

---Get a position inside or on the Input Bounds
---@param aa vector lower-bound
---@param bb vector upper-bound
---@param vec vector|nil A normalized Vector pointing towards the position that should be retrieved, Default is Vec(0, 0, 0)
---@return vector
function AutoAABBGetPos(aa, bb, vec)
	vec = AutoDefault(vec, Vec(0, 0, 0))

	vec = AutoVecMap(vec, -1, 1, 0, 1)
	local sizevec = VecSub(bb, aa)

	local size = VecLength(sizevec)
	local scaled = AutoVecMulti(vec, sizevec)
	return VecAdd(scaled, aa)
end

---Get the corners of the given Bounds
---@param aa vector lower-bound
---@param bb vector upper-bound
---@return table
function AutoAABBGetCorners(aa, bb)
	local mid = {}
	for i = 1, 3 do
		mid[i] = (aa[i] + bb[i]) / 2
	end

	local corners = {
		{ bb[1], mid[2], mid[3] },
		{ aa[1], mid[2], mid[3] },
		{ mid[1], bb[2], mid[3] },
		{ mid[1], aa[2], mid[3] },
		{ mid[1], mid[2], bb[3] },
		{ mid[1], mid[2], aa[3] },
		{ aa[1], bb[2], mid[3] },
		{ bb[1], aa[2], mid[3] }
	}

	return corners
end

---Get data about the size of the given Bounds
---@param aa vector lower-bound
---@param bb vector upper-bound
---@return table representing the size of the Bounds
---@return number smallest smallest edge size of the Bounds
---@return number longest longest edge size of the Bounds
function AutoAABBSize(aa, bb)
	local size = VecSub(bb, aa)
	local minval = math.min(unpack(size))
	local maxval = math.max(unpack(size))

	return size, minval, maxval
end

---Takes a given AABB and subdivides into new AABBs
---@param aa vector lower-bound
---@param bb vector upper-bound
---@param levels number?
---@return table
function AutoSubdivideBounds(aa, bb, levels)
	levels = levels or 1
	local bounds = { { aa, bb } }

	for level = 1, levels do
		local newBounds = {}

		for _, bound in ipairs(bounds) do
			local mid = {}
			for i = 1, 3 do
				mid[i] = (bound[1][i] + bound[2][i]) / 2
			end

			table.insert(newBounds, { { bound[1][1], mid[2], mid[3] }, { mid[1], bound[2][2], bound[2][3] } })
			table.insert(newBounds, { { mid[1], mid[2], mid[3] }, { bound[2][1], bound[2][2], bound[2][3] } })
			table.insert(newBounds, { { mid[1], bound[1][2], mid[3] }, { bound[2][1], mid[2], bound[2][3] } })
			table.insert(newBounds, { { bound[1][1], bound[1][2], mid[3] }, { mid[1], mid[2], bound[2][3] } })
			table.insert(newBounds, { { bound[1][1], mid[2], bound[1][3] }, { mid[1], bound[2][2], mid[3] } })
			table.insert(newBounds, { { mid[1], mid[2], bound[1][3] }, { bound[2][1], bound[2][2], mid[3] } })
			table.insert(newBounds, { { mid[1], bound[1][2], bound[1][3] }, { bound[2][1], mid[2], mid[3] } })
			table.insert(newBounds, { { bound[1][1], bound[1][2], bound[1][3] }, { mid[1], mid[2], mid[3] } })
		end

		bounds = newBounds
	end

	return bounds
end

---Draws a given Axis Aligned Bounding Box
---@param aa vector lower-bound
---@param bb vector upper-bound
---@param colorR number
---@param colorG number
---@param colorB number
---@param alpha number
---@param rgbcolors boolean?
---@param draw boolean?
function AutoDrawAABB(aa, bb, colorR, colorG, colorB, alpha, rgbcolors, draw)
	colorR = AutoDefault(colorR, 0)
	colorG = AutoDefault(colorG, 0)
	colorB = AutoDefault(colorB, 0)
	alpha = AutoDefault(alpha, 1)
	rgbcolors = AutoDefault(rgbcolors, false)
	draw = AutoDefault(draw, false)

	min, max = {
		[1] = Vec(aa[1], aa[2], aa[3]),
		[2] = Vec(bb[1], aa[2], aa[3]),
		[3] = Vec(bb[1], aa[2], bb[3]),
		[4] = Vec(aa[1], aa[2], bb[3]),
	}, {
		[1] = Vec(aa[1], bb[2], aa[3]),
		[2] = Vec(bb[1], bb[2], aa[3]),
		[3] = Vec(bb[1], bb[2], bb[3]),
		[4] = Vec(aa[1], bb[2], bb[3]),
	}

	-- This code made me want to give up
	local lines = {
		{ min[2], min[3], colorR, colorG, colorB, alpha },
		{ min[3], min[4], colorR, colorG, colorB, alpha },
		{ max[1], max[2], colorR, colorG, colorB, alpha },
		{ max[4], max[1], colorR, colorG, colorB, alpha },
		{ min[2], max[2], colorR, colorG, colorB, alpha },
		{ min[4], max[4], colorR, colorG, colorB, alpha },

		{ min[1], min[2], rgbcolors and 1 or colorR, rgbcolors and 0 or colorG, rgbcolors and 0 or colorB, alpha },
		{ max[2], max[3], rgbcolors and 0 or colorR, rgbcolors and 1 or colorG, rgbcolors and 0 or colorB, alpha },
		{ max[3], max[4], rgbcolors and 1 or colorR, rgbcolors and 0 or colorG, rgbcolors and 0 or colorB, alpha },
		{ min[1], max[1], rgbcolors and 0 or colorR, rgbcolors and 0 or colorG,
			rgbcolors and 1 or rgbcolors and 0 or colorB, alpha },
		{ min[3], max[3], rgbcolors and 0 or colorR, rgbcolors and 0 or colorG,
			rgbcolors and 1 or rgbcolors and 0 or colorB, alpha },
		{ min[4], min[1], rgbcolors and 0 or colorR, rgbcolors and 1 or colorG, rgbcolors and 0 or colorB, alpha },
	}

	local DrawLine = DrawLine
	local DebugLine = DebugLine

	for i, v in ipairs(lines) do
		if draw then
			DrawLine(unpack(v))
		else
			DebugLine(unpack(v))
		end
	end
end

---Converts an Axis Aligned Bounding Box to a Oriented Bounding Box
---@param aa vector
---@param bb vector
---@return OBB
function AutoAABBToOBB(aa, bb)
    local center = VecLerp(bb, aa, 0.5)
    local size = VecSub(bb, aa)
    return { pos = center, rot = QuatEuler(), size = size }
end

---Defines a Oriented Bounding Box
---@param center vector
---@param rot quaternion
---@param size vector|number?
---@return table
function AutoOBB(center, rot, size)
    return {
        pos = center or Vec(),
        rot = rot or QuatEuler(),
		size = type(size) == 'table' and size or AutoVecOne(size or 1)
	}
end

---Returns the corners of a Oriented Bounding Box
---@param obb OBB
---@return { xyz:table, Xyz:table, xYz:table, xyZ:table, XYz:table, XyZ:table, xYZ:table, XYZ:table }
function AutoGetOBBCorners(obb)
	local corners = {}

	-- Calculate the eight corner points of the OBB based on the center, dimensions, and orientation
	local hs = VecScale(obb.size, 0.5)
	corners.xyz = TransformToParentPoint(obb, VecScale(hs, -1))
	corners.Xyz = TransformToParentPoint(obb, Vec(hs[1], -hs[2], -hs[3]))
	corners.xYz = TransformToParentPoint(obb, Vec(-hs[1], hs[2], -hs[3]))
	corners.xyZ = TransformToParentPoint(obb, Vec(-hs[1], -hs[2], hs[3]))
	corners.XYz = TransformToParentPoint(obb, Vec(hs[1], hs[2], -hs[3]))
	corners.XyZ = TransformToParentPoint(obb, Vec(hs[1], -hs[2], hs[3]))
	corners.xYZ = TransformToParentPoint(obb, Vec(-hs[1], hs[2], hs[3]))
	corners.XYZ = TransformToParentPoint(obb, hs)

	return corners
end

---Returns the corners and transforms representing the faces of a Oriented Bounding Box---@param obb OBB
---@param obb OBB
---@return { z:{ pos:table, rot:table, size:table }, zn:{ pos:table, rot:table, size:table }, x:{ pos:table, rot:table, size:table }, xn:{ pos:table, rot:table, size:table }, y:{ pos:table, rot:table, size:table }, yn:{ pos:table, rot:table, size:table } }
---@return { xyz:table, Xyz:table, xYz:table, xyZ:table, XYz:table, XyZ:table, xYZ:table, XYZ:table }
function AutoGetOBBFaces(obb)
	local corners = AutoGetOBBCorners(obb)

	local faces = {}
	faces.z = {
		pos = VecLerp(corners.xyZ, corners.XYZ, 0.5),
		rot = QuatRotateQuat(obb.rot, QuatEuler(180, 0, 0)),
		size = { obb.size[1], obb.size[2] }
	}
	faces.zn = {
		pos = VecLerp(corners.xyz, corners.XYz, 0.5),
		rot = QuatRotateQuat(obb.rot, QuatEuler(0, 0, 0)),
		size = { obb.size[1], obb.size[2] }
	}
	faces.x = {
		pos = VecLerp(corners.Xyz, corners.XYZ, 0.5),
		rot = QuatRotateQuat(obb.rot, QuatEuler(0, -90, -90)),
		size = { obb.size[2], obb.size[3] }
	}
	faces.xn = {
		pos = VecLerp(corners.xyz, corners.xYZ, 0.5),
		rot = QuatRotateQuat(obb.rot, QuatEuler(0, 90, 90)),
		size = { obb.size[2], obb.size[3] }
	}
	faces.y = {
		pos = VecLerp(corners.xYz, corners.XYZ, 0.5),
		rot = QuatRotateQuat(obb.rot, QuatEuler(90, 0, 0)),
		size = { obb.size[1], obb.size[3] }
	}
	faces.yn = {
		pos = VecLerp(corners.xyz, corners.XyZ, 0.5),
		rot = QuatRotateQuat(obb.rot, QuatEuler(-90, 180, 0)),
		size = { obb.size[1], obb.size[3] }
	}

	return faces, corners
end

function AutoOBBLines(obb)
	local c = AutoGetOBBCorners(obb)

	return {
		{ c.xyz, c.Xyz },
		{ c.xYz, c.XYz },
		{ c.xyZ, c.XyZ },
		{ c.xYZ, c.XYZ },

		{ c.xyz, c.xYz },
		{ c.Xyz, c.XYz },
		{ c.xyZ, c.xYZ },
		{ c.XyZ, c.XYZ },

		{ c.xyz, c.xyZ },
		{ c.Xyz, c.XyZ },
		{ c.xYz, c.xYZ },
		{ c.XYz, c.XYZ },
	}
end

---Draws a given Oriented Bounding Box
---@param obb OBB
---@param red number? Default is 0
---@param green number? Default is 0
---@param blue number? Default is 0
---@param alpha number? Default is 1
function AutoDrawOBB(obb, red, green, blue, alpha)
	local lines = AutoOBBLines(obb)

	for k, l in pairs(lines) do
		DebugLine(l[1], l[2], red or 0, green or 0, blue or 0, alpha or 1)
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Octree Functions-----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoProcessOctree(BoundsAA, BoundsBB, Layers, conditionalFuction, _layer)
	_layer = _layer or 1
	if _layer >= (Layers or 5) + 1 then return end

	conditionalFuction = AutoDefault(conditionalFuction, AutoQueryBoundsForBody)

	local check, querydata = conditionalFuction(BoundsAA, BoundsBB)
	local node = {
		aa = BoundsAA,
		bb = BoundsBB,
		check = check,
		querydata = querydata,
		layer = _layer,
		children = {},
	}

	if check then
		for _, nb in ipairs(AutoSubdivideBounds(BoundsAA, BoundsBB)) do
			local aa, bb = unpack(nb)
			node.children[#node.children + 1] = AutoProcessOctree(aa, bb, Layers, conditionalFuction, _layer + 1)
		end
	end

	return node
end

function AutoQueryBoundsForBody(aa, bb)
	QueryRequire('physical large')
	local mid = VecLerp(aa, bb, 0.5)
	local radius = AutoVecDist(aa, bb) * 0.707 / 2
	local hit, point, normal, shape = QueryClosestPoint(mid, radius)
	return hit, { pos = point, normal = normal, shape }
end

function AutoDrawOctree(node, layer, drawfunction)
	if node == nil then return end

	if layer then
		if node.layer > layer then
			return
		end
	end

	if not drawfunction then
		if node.check and (not layer or (node.layer == layer)) then
			local c1, c2, c3 = AutoHSVToRGB(node.layer / 10, 1, 1)
			AutoDrawAABB(node.aa, node.bb, c1, c2, c3, 1)
		end
	elseif not layer or (node.layer == layer) then
		drawfunction(node, layer)
	end

	for _, child in ipairs(node.children) do
		AutoDrawOctree(child, layer, drawfunction)
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Point Physics------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoSimInstance()
	local t = {
		Points = {

		},
		Settings = {
			Steps = 1,
			PointsAffectBodies = true,
		}
	}

	---Creates a Point to be Simulated with SimInstance:CreatePoint(), you can add parameters after it is created and change existing ones, such as point.reflectivity, and point.mass
	---@param Position vector|nil Default is Vec(0, 0, 0)
	---@param Velocity vector|nil Default is Vec(0, 0, 0)
	---@return table point
	---@return number newindex
	function t:CreatePoint(Position, Velocity)
		local new_point = {}
		new_point.pos = AutoDefault(Position, { 0, 0, 0 })
		new_point.vel = AutoDefault(Velocity, { 0, 0, 0 })
		new_point.acc = Vec(0, 0, 0)

		new_point.radius = 0
		new_point.mass = 1
		new_point.drag = 0.0
		new_point.reflectivity = 0
		new_point.gravity = Vec(0, -10, 0)

		new_point.collision = true
		new_point.simulate = true
		new_point.presimulate = nil
		new_point.draw = true
		new_point.remove = nil

		new_point.color = { 1, 1, 1, 1 }

		local new_index = #self.Points + 1
		self.Points[new_index] = new_point

		return new_point, new_index
	end

	---Updates all of the point in the Simulation
	---@param dt number The timestep that is used. Default is GetTimeStep()
	function t:Simulate(dt, showsteps)
		dt = AutoDefault(dt, GetTimeStep()) / self.Settings.Steps
		showsteps = AutoDefault(showsteps, false)

		for _ = 1, not showsteps and self.Settings.Steps or 1 do
			-- Update Points
			for i, p in ipairs(self.Points) do
				if p.simulate then
					AutoExecute(p.presimulate, p, i, dt)

					local new_pos = VecAdd(VecAdd(p.pos, VecScale(p.vel, dt)), VecScale(p.acc, dt ^ 2 * 0.5))
					local new_acc = (function()
						local grav_acc = VecCopy(p.gravity)
						local drag_force = VecScale(p.vel, 0.5 * p.drag)
						local drag_acc = AutoVecDiv(drag_force, AutoVecOne(p.mass))
						return VecSub(grav_acc, drag_acc)
					end)()
					local new_vel = VecAdd(p.vel, VecScale(VecAdd(p.acc, new_acc), dt * 0.5))

					local collided = false
					local collisiondata = {}

					if p.collision then
						local diff = VecSub(new_pos, p.pos)
						local dir = VecNormalize(diff)
						local hit, dist, normal, shape = QueryRaycast(new_pos, dir, VecLength(diff) + p.radius)
						if hit then
							local colpoint = VecAdd(new_pos, VecScale(dir, dist))
							local precol_vel = VecCopy(new_vel)

							local dot = VecDot(new_vel, normal)
							local incoming = -VecDot(VecNormalize(precol_vel), normal)

							new_pos = VecAdd(VecScale(diff, math.min(dist, VecLength(diff))), p.pos)

							new_vel = VecScale(VecSub(new_vel, VecScale(normal, dot * 2)), p.reflectivity)
							new_vel = VecAdd(new_vel, VecScale(GetBodyVelocity(GetShapeBody(shape)), 2))

							if self.Settings.PointsAffectBodies then
								ApplyBodyImpulse(GetShapeBody(shape), colpoint, VecScale(precol_vel, incoming * p.mass))
							end

							collided = true
							collisiondata = {
								location = colpoint,
								normal = normal,
								shape = shape,
								incoming = incoming,
								vel = VecCopy(p.vel)
							}
						end
					end

					p.pos = new_pos
					p.vel = new_vel
					p.acc = new_acc

					if collided then AutoExecute(p.collision, p, i, collisiondata) end
					AutoExecute(p.simulate, p, i, dt)
				end
			end
		end
	end

	---Removes a point from the Simulation by it's index, Calls point.remove if it is a function or table of functions
	---@param index table The Index of the Point that should be removed
	function t:Remove(index)
		AutoExecute(self.Points[index].remove, self.Points[index])

		-- Cleanup
		for i, v in pairs(self.Points[index]) do
			v = nil
			self.Points[index][i] = nil
		end

		table.remove(self.Points, index)
	end

	---Iterate through every Point
	---@param func function The Function that is called, called with the input parameter of (Point, Index)
	function t:Do(func)
		for i, p in ipairs(self.Points) do
			AutoExecute(func, p, i)
		end
	end

	---Draw every point in which point.draw is not false (by default, point.draw is true), calling p.draw at the end if it is a function
	---@param Image string|false|nil A image that is drawn in the position of the points, Default is 'ui/common/dot.png', if set to false, then draws a Transform at the position instead
	---@param SizeMultiplier number|nil a multipler for the size of the drawn image, Default is 3.5
	---@param Occlude boolean|nil Whether to hide points that are obscured by walls, Default is true
	function t:Draw(Image, SizeMultiplier, Occlude)
		Image = AutoDefault(Image, 'ui/common/dot.png')
		SizeMultiplier = AutoDefault(SizeMultiplier, 3.5)
		Occlude = AutoDefault(Occlude, true)

		local inview = true

		for i, p in ipairs(self.Points) do
			if p.draw then
				local inview, angle, dist = AutoPointInView(p.pos, nil, nil, Occlude)
				if inview then
					local x, y = UiWorldToPixel(p.pos)
					local size = 1 / dist * SizeMultiplier

					if not Image then
						AutoDrawTransform(Transform(p.pos, p.rot and p.rot or QuatEuler(0, 0, 0)), size / 10)
					else
						UiPush()
						if p.color then UiColor(p.color[1], p.color[2], p.color[3], p.color[4]) end
						UiAlign('center middle')
						UiTranslate(x, y)

						UiScale(size)
						UiImage(Image)
						UiPop()
					end
				end

				AutoExecute(p.draw, p, i, inview)
			end
		end
	end

	return t
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Secondary Motion-----Previously Second Order System--------------Huge Thanks to Mathias#1325 for work on the Quaternion Functions------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Returns a table representing a Second Order System (SOS) that can be used to make secondary motion
---@param initial number|table<number>
---@param frequency number
---@param dampening number
---@param response number
---@param raw_k boolean?
---@return { data:{ current:number, previous:number, velocity:number }|table<{ current:number, previous:number, velocity:number }>, k_values:{ [1]:number, [2]:number, [3]:number} }
function AutoSM_Define(initial, frequency, dampening, response, raw_k)
	local sosdata = {
		type = type(initial) == 'table' and 'table' or 'single',
		data = {},
		k_values = {
			raw_k and frequency or (dampening / (math.pi * frequency)),
			raw_k and dampening or (1 / (2 * math.pi * frequency) ^ 2),
			raw_k and response or (response * dampening / (2 * math.pi * frequency)),
		}
	}

	if sosdata.type ~= 'single' then
		for k, v in pairs(initial) do
			sosdata.data[k] = {
				current = v,
				previous = v,
				velocity = 0
			}
		end
	else
		sosdata.data = {
			current = initial,
			previous = initial,
			velocity = 0
		}
	end
	

	return sosdata
end

function AutoSM_DefineQuat(initial, frequency, dampening, response, raw_k)
    local sosdata = {
		type = 'quaternion',
		data = {
			current = QuatCopy(initial),
			previous = QuatCopy(initial),
			velocity = Vec(), -- Angular velocity as a vector
		},
		k_values = {
			raw_k and frequency or (dampening / (math.pi * frequency)),
			raw_k and dampening or (1 / ((2 * math.pi * frequency) ^ 2)),
			raw_k and response or (dampening * response / (2 * math.pi * frequency)),
		}
	}

	return sosdata
end

---Updates the state of the Second Order System (SOS) towards the target value, over the specified timestep.
---This function is used in conjunction with the AutoSM_Define
---@param sos any
---@param target any
---@param timestep any
function AutoSM_Update(sos, target, timestep)
    timestep = timestep or GetTimeStep()

    if sos.type ~= 'quaternion' then
        local function update(v, t)
            local xd = (t - v.previous) / timestep
            v.previous = t

            local k2_stable = math.max(sos.k_values[2], timestep ^ 2 / 2 + timestep * sos.k_values[1] / 2,
            timestep * sos.k_values[1])
            v.current = v.current + timestep * v.velocity
            v.velocity = v.velocity +
            timestep * (t + sos.k_values[3] * xd - v.current - sos.k_values[1] * v.velocity) / k2_stable
        end

        if sos.type == 'single' then
            update(sos.data, target)
        else
            for k, v in pairs(sos.data) do
                update(v, target[k])
            end
        end
    else
		-- Compute the quaternion that will rotate the last quaternion to the desired quaternion
		-- Convert it to an axis-angle rotation vector
		local q = QuatRotateQuat(AutoQuatConjugate(sos.data.previous), AutoQuatNearest(target, sos.data.previous))
		local dx = AutoQuatToAxisAngle(q)
		dx = VecScale(dx, 1 / timestep)
		
		sos.data.previous = QuatCopy(target)

		-- Convert our angular velocity to a quaternion
		local qVel = AutoQuatFromAxisAngle(VecScale(sos.data.velocity, timestep))
		sos.data.current = QuatRotateQuat(sos.data.current, qVel) -- Rotate

		-- desired - sos.data.current, in quaternion form
		local q2 = QuatRotateQuat(AutoQuatConjugate(sos.data.current), AutoQuatNearest(target, sos.data.current))
		local s = AutoQuatToAxisAngle(q2)
		local k2_stable = math.max(sos.k_values[2], timestep * timestep / 2 + timestep * sos.k_values[1] / 2, timestep * sos.k_values[1])

		--- "wtf" - Autumn
		sos.data.velocity = VecAdd(sos.data.velocity, VecScale(VecScale(VecAdd(s, VecSub(VecScale(dx, sos.k_values[3]), VecScale(sos.data.velocity, sos.k_values[1]))), timestep), 1 / k2_stable))
    end
end

---Just gets the current value of a Second Order System
---@return number|table|quaternion
function AutoSM_Get(sos)
	if sos.type ~= 'table' then
		return sos.data.current
	else
		local values = {}
		for k, v in pairs(sos.data) do
			values[k] = v.current
		end

		return values
	end
end

function AutoSM_GetVelocity(sos)
	if sos.type ~= 'table' then
		return sos.data.velocity
	else
		return AutoTableSubi(sos.data, 'velocity')
	end
end

function AutoSM_Set(sos, target, keep_velocity)
	if sos.type ~= 'table' then
		sos.data.current = target
		sos.data.previous = target
		if keep_velocity then sos.data.velocity = 0 end
	else
		for k, v in pairs(sos.data) do
			v.current = target[k]
			v.previous = target[k]
			if keep_velocity then v.velocity = 0 end
		end
	end
end

function AutoSM_SetVelocity(sos, velocity)
	if sos.type ~= 'table' then
		sos.data.velocity = velocity
	else
		for k, v in pairs(sos.data) do
			v.velocity = velocity[k]
		end
	end
end

function AutoSM_AddVelocity(sos, velocity)
	if sos.type == 'single' then
		sos.data.velocity = sos.data.velocity + velocity
	elseif sos.type == 'quaternion' then
		sos.data.velocity = VecAdd(sos.data.velocity, AutoEulerTable(velocity))
	else
		for k, v in pairs(sos.data) do
			v.velocity = v.velocity + velocity[k]
		end
	end
end

function AutoSM_RecalculateK(sos, frequency, dampening, response, raw_k)
	sos.k_values = {
		raw_k and frequency or (dampening / (math.pi * frequency)),
		raw_k and dampening or (1 / (2 * math.pi * frequency) ^ 2),
		raw_k and response or (response * dampening / (2 * math.pi * frequency)),
	}
end

-- ---Sets up a Second Order System, using code by t3ssel8r
-- ---@param inital number|table The inital current value of the system
-- ---@param frequency number The frequency in which the system will respond to input
-- ---@param zeta number
-- ---@param response number
-- function AutoCreateSOS(inital, frequency, zeta, response)
-- 	local t = {}
-- 	t.value = inital
-- 	t.last = inital
-- 	t.vel = 0

-- 	t.k1 = zeta / (math.pi * frequency)
-- 	t.k2 = 1 / ((2 * math.pi * frequency) ^ 1)
-- 	t.k3 = response * zeta / (2 * math.pi * frequency)

-- 	return t
-- end

-- function AutoSOSUpdate(sos, desired, time)
-- 	time = AutoDefault(time, GetTimeStep())
-- 	local xd = (desired - sos.last) / time
-- 	sos.last = desired

-- 	local k2_stable = math.max(sos.k2, time ^ 2 / 2 + time * sos.k1 / 2, time * sos.k1)
-- 	sos.value = sos.value + time * sos.vel
-- 	sos.vel = sos.vel + time * (desired + sos.k3 * xd - sos.value - sos.k1 * sos.vel) / k2_stable
-- end

-- function AutoCreateSOSBatch(initaltable, frequency, dampening, response)
-- 	local sos = { value = nil }

-- 	for k, v in pairs(initaltable) do
-- 		sos[k] = AutoCreateSOS(v or 0, frequency, dampening, response)
-- 	end
	
-- 	return setmetatable(sos, {
-- 		__index = function(t, k)
-- 			if k == 'value' then
-- 				local v = {}
-- 				for k2, v2 in pairs(t) do
-- 					v[k2] = v2.value
-- 				end

-- 				return v
-- 			end
-- 		end,
-- 		__newindex = function(t, k, v)
-- 			if k == 'value' then
-- 				for i = 1, #v do
-- 					local s = rawget(t, i)
-- 					s.value = v[i]
-- 					s.last = v[i]
-- 				end
-- 			end
-- 		end
-- 	})
-- end

-- function AutoSOSUpdateBatch(sostable, desired, time)
-- 	for i, v in pairs(sostable) do
-- 		local d = type(desired) == 'table' and AutoDefault(desired[i], v.value) or (desired or v.value)
-- 		AutoSOSUpdate(v, d, time)
-- 	end
-- end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Table Functions------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Returns the amount of elements in the given list.
---@param t table
---@return integer
function AutoTableCount(t)
	local c = 0
	for i in pairs(t) do
		c = c + 1
	end

	return c
end

function AutoTableRepeatValue(v, r)
	local t = {}
	for i=1,r do
		t[#t+1] = type(v) == 'table' and AutoTableDeepCopy(v) or v
	end
	return t
end

---Concats Table 2 onto the end of Table 1, does not return anything
---@param t1 any
---@param t2 any
function AutoTableConcat(t1, t2)
	for i = 1, #t2 do
		t1[#t1 + 1] = t2[i]
	end
end

function AutoTableMerge(base, overwrite)
	for k, v in pairs(overwrite) do
		if type(v) == "table" then
			if type(base[k] or false) == "table" then
				AutoTableMerge(base[k], v)
			else
				base[k] = v
			end
		else
			base[k] = v
		end
	end
	return base
end

function AutoTableSub(t, key)
	local _t = {}
	for i, v in pairs(t) do
		_t[i] = v[key]
	end
	return _t
end

---Same as AutoTableSub, but uses ipairs instead
---@param t any
---@param key any
---@return table
function AutoTableSubi(t, key)
	local _t = {}
	for i, v in ipairs(t) do
		_t[i] = v[key]
	end
	return _t
end

function AutoTableListKeys(t)
	local _t = {}
	for k, _ in pairs(t) do
		_t[#_t+1] = k
	end
	return _t
end

function AutoTableSwapKeysAndValues(t)
	local _t = {}
	for k, v in pairs(t) do
		_t[v] = k
	end
	return _t
end

function AutoTableAppend(t, key, tset)
	for i, v in pairs(t) do
		v[key] = tset[i]
	end
end

---Returns true and the index if the v is in t, otherwise returns false and nil
---@param t table
---@param v any
---@return boolean, unknown
function AutoTableContains(t, v)
	for i, v2 in ipairs(t) do
		if v == v2 then
			return true, i
		end
	end
	return false, nil
end

---Returns the Last item of a given list
---@param t any
---@return unknown
function AutoTableLast(t)
	return t[AutoTableCount(t)]
end

---Copy a Table Recursivly Stolen from http://lua-users.org/wiki/CopyTable
---@param orig table
---@param copies table?
---@return table
function AutoTableDeepCopy(orig, copies)
	copies = copies or {}
	local orig_type = type(orig)
	local copy
	if orig_type == 'table' then
		if copies[orig] then
			copy = copies[orig]
		else
			copy = {}
			copies[orig] = copy
			for orig_key, orig_value in next, orig, nil do
				copy[AutoTableDeepCopy(orig_key, copies)] = AutoTableDeepCopy(orig_value, copies)
			end
			setmetatable(copy, AutoTableDeepCopy(getmetatable(orig), copies))
		end
	else -- number, string, boolean, etc
		copy = orig
	end
	return copy
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Utility Functions------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---If val is nil, return default instead
---@param v any
---@param default any
---@return any
function AutoDefault(v, default)
	if v == nil then return default else return v end
end

function AutoExecute(f, ...)
	if not f then return end

	if type(f) == "function" then
		f(unpack(arg))
	elseif type(f) == "table" then
		for _, sub_f in pairs(f) do
			if type(sub_f) == "function" then
				sub_f(unpack(arg))
			end
		end
	end
end

---Calls VecLerp on a table of Vectors
---@param a table A table of Vectors
---@param b table A table of Vectors the same size of a
---@param t number
---@return table
function AutoVecTableLerp(a, b, t)
	local c = {}
	for k, _ in pairs(a) do
		c[k] = VecLerp(a[k], b[k], t)
	end
	return c
end

---Calls VecLerp on a table of Vectors
---@param a table A table of values
---@param b table A table of values the same size of a
---@param t number
---@return table
function AutoTableLerp(a, b, t)
	local c = {}
	for k, _ in pairs(a) do
		c[k] = AutoLerp(a[k], b[k], t)
	end
	return c
end

function AutoXmlToNumbers(xmlString)
	local st = AutoSplit(xmlString, ' ')
	local t = {}
	for i, v in ipairs(st) do
		t[i] = tonumber(v)
	end
	return t
end

---Returns a Linear Interpolated Transform, Interpolated by t.
---@param a Transformation
---@param b Transformation
---@param t number
---@param t2 number|nil
---@return table
function AutoTransformLerp(a, b, t, t2)
	if t2 == nil then
		t2 = t
	end
	return Transform(
		VecLerp(a.pos, b.pos, t),
		QuatSlerp(a.rot, b.rot, t2)
	)
end

function AutoTransformFwd(t, scale)
	scale = AutoDefault(scale, 1)
	return QuatRotateVec(t.rot, Vec(0, 0, -scale))
end

function AutoTransformUp(t, scale)
	scale = AutoDefault(scale, 1)
	return QuatRotateVec(t.rot, Vec(0, scale))
end

function AutoTransformRight(t, scale)
	scale = AutoDefault(scale, 1)
	return QuatRotateVec(t.rot, Vec(scale))
end

function AutoTransformOffset(t, offset)
	return Transform(TransformToParentPoint(t, offset), t.rot)
end

function AutoEulerTable(quat)
	local x, y, z = GetQuatEuler(quat)
	return Vec(x, y, z)
end

function AutoEulerTransform(eulerTransform)
	return {
		pos = VecCopy(eulerTransform.pos),
		rot = QuatEuler(unpack(eulerTransform.rot))
	}
end

---Returns a Vector for easy use when put into a parameter for xml
---@param vec any
---@param round number
---@return string
function AutoVecToXML(vec, round)
	round = AutoDefault(round, 0)
	return AutoRound(vec[1], round) .. ' ' .. AutoRound(vec[2], round) .. ' ' .. AutoRound(vec[3], round)
end

---A workaround to making a table readonly, don't use, it most likely is bugged in someway
---@param t table
---@return table
function AutoSetReadOnly(t)
	return setmetatable({}, {
		__index = t,
		__newindex = function(t, key, value)
			error("attempting to change constant " .. tostring(key) .. " to " .. tostring(value), 2)
		end
	})
end

---Splits a string by a separator
---@param inputstr string
---@param sep string
---@return table
function AutoSplit(inputstr, sep, number)
	if sep == nil then
		sep = "%s"
	end
	local t = {}
	for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
		table.insert(t, number and tonumber(str) or str)
	end
	return t
end

function AutoCamelCase(str)
	local subbed = str:gsub('_', ' ')
	return string.gsub(" " .. subbed, "%W%l", string.upper):sub(2)
end

---Returns 3 values from HSV color space from RGB color space
---@param hue number|nil The hue from 0 to 1
---@param sat number|nil The saturation from 0 to 1
---@param val number|nil The value from 0 to 1
---@return number, number, number Returns the red, green, blue of the given hue, saturation, value
function AutoHSVToRGB(hue, sat, val)
	local r, g, b

	local i = math.floor(hue * 6);
	local f = hue * 6 - i;
	local p = val * (1 - sat);
	local q = val * (1 - f * sat);
	local t = val * (1 - (1 - f) * sat);

	i = i % 6

	if i == 0 then r, g, b = val, t, p
	elseif i == 1 then r, g, b = q, val, p
	elseif i == 2 then r, g, b = p, val, t
	elseif i == 3 then r, g, b = p, q, val
	elseif i == 4 then r, g, b = t, p, val
	elseif i == 5 then r, g, b = val, p, q
	end

	return r, g, b
end

---Returns 3 values from RGB color space from HSV color space
---@param r number|nil The red from 0 to 1
---@param g number|nil The green from 0 to 1
---@param b number|nil The blue from 0 to 1
---@return number, number, number Returns the hue, the saturation, and the value
function AutoRGBToHSV(r, g, b)
	r, g, b = r, g, b
	local max, min = math.max(r, g, b), math.min(r, g, b)
	local h, s, v
	v = max

	local d = max - min
	if max == 0 then s = 0 else s = d / max end

	if max == min then
		h = 0 -- achromatic
	else
		if max == r then
			h = (g - b) / d
			if g < b then h = h + 6 end
		elseif max == g then h = (b - r) / d + 2
		elseif max == b then h = (r - g) / d + 4
		end
		h = h / 6
	end

	return h, s, v
end

function AutoHEXtoRGB(hex)
	local function f(x, p)
		x = x:gsub("#", "")
		local r, g, b = tonumber("0x" .. x:sub(1, 2)) / 255, tonumber("0x" .. x:sub(3, 4)) / 255,
			tonumber("0x" .. x:sub(5, 6)) / 255
		if p then return { r, g, b } else return r, g, b end
	end

	if type(hex) == 'string' then return f(hex) else
		local t = {}
		for key, val in pairs(hex) do
			t[key] = f(val, true)
		end
		return t
	end
end

function AutoStringToByteTable(str)
	local t = {}
	for i = 1, #str do
		t[i] = (str:sub(i, i)):byte()
	end
	return t
end

function AutoByteTableToString(t)
	local str = ''
	for i, b in ipairs(t) do
		str = str .. string.char(b)
	end
	return str
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Game Functions-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Usually, the Primary Menu Button only is suppose to work in the mod's level, this is a work around to have it work in any level.
---@param title any
---@return boolean
function AutoPrimaryMenuButton(title)
	local value = PauseMenuButton(title, true)

	for _, item in pairs(ListKeys('game.pausemenu.items')) do
		if GetString(AutoKey('game.pausemenu.items', item)) == title then
			SetInt('game.pausemenu.primary', item)
			break
		end
	end

	return value
end

---Goes through a table and performs Delete() on each element
---@param t any
function AutoDeleteHandles(t, CheckIfValid)
	local list = {}
	for k, v in pairs(t) do
		local valid = IsHandleValid(v)
		list[#list+1] = { handle = v, type = GetEntityType(v), valid = valid }

		if not CheckIfValid or (valid and v ~= GetWorldBody()) then
			Delete(v)
		end
	end

	return list
end

---@param t any
function AutoListHandleTypes(t)
	local nt = {}
	for key, value in pairs(t) do
		nt[key] = { handle = value, type = IsHandleValid(value) and GetEntityType(value) or 'Invalid handle' }
	end
	return nt
end

-- Might be broken, didn't test
function AutoSpawnScript(path, ...)
	local f = [[<script file="%s" param0="%s" param1="%s" param2="%s" param3="%s"/>]]
	local param = { arg[1] or '', arg[2] or '', arg[3] or '', arg[4] or '' }
	return Spawn((f):format(path, unpack(param)), Transform())[1]
end

---QueryRaycast with some extra features; Also puts everything into a table
---@param origin table
---@param direction table
---@param maxDist number
---@param radius number?
---@param rejectTransparent boolean?
---@return { hit:boolean, dist:number, normal:table, shape:shape_handle, intersection:table, dot:number, reflection:table }
function AutoRaycast(origin, direction, maxDist, radius, rejectTransparent)
	direction = direction and VecNormalize(direction) or nil
	
	local data = {}
	data.hit, data.dist, data.normal, data.shape = QueryRaycast(origin, direction, maxDist or 256, radius, rejectTransparent)
	data.intersection = VecAdd(origin, VecScale(direction, data.dist))
	data.dot = VecDot(direction, data.normal)
	data.reflection = VecSub(direction, VecScale(data.normal, data.dot * 2))

	return data
end

---AutoRaycast from point A to point B. The distance will default to the distance between the points, but can be set.
---@param pointA vector
---@param pointB vector
---@param manualDistance number?
---@param radius number?
---@param rejectTransparent boolean?
---@return { hit:boolean, dist:number, normal:vector, shape:shape_handle, intersection:vector, dot:number, reflection:vector }
function AutoRaycastTo(pointA, pointB, manualDistance, radius, rejectTransparent)
	local diff = VecSub(pointB, pointA)
	return AutoRaycast(pointA, diff, manualDistance or VecLength(diff), radius, rejectTransparent)
end

---@return { hit:boolean, dist:number, normal:vector, shape:shape_handle, intersection:vector, dot:number, reflection:vector }, table cameraTransform, table cameraForward
function AutoRaycastCamera(usePlayerCamera, maxDist, radius, rejectTransparent)
	local trans = usePlayerCamera and GetPlayerCameraTransform() or GetCameraTransform()
	local fwd = AutoTransformFwd(trans)

	return AutoRaycast(trans.pos, fwd, maxDist, radius, rejectTransparent), trans, fwd
end

function AutoRaycastPlane(startPos, direction, planeTransform, size, oneway)
	local halfsize = VecScale(size, 0.5)
	local corner1 = VecAdd(planeTransform.pos, QuatRotateVec(planeTransform.rot, Vec(-halfsize[1], -halfsize[2], 0)))
	local corner2 = VecAdd(planeTransform.pos, QuatRotateVec(planeTransform.rot, Vec(halfsize[1], -halfsize[2], 0)))
	local corner3 = VecAdd(planeTransform.pos, QuatRotateVec(planeTransform.rot, Vec(halfsize[1], halfsize[2], 0)))
	local corner4 = VecAdd(planeTransform.pos, QuatRotateVec(planeTransform.rot, Vec(-halfsize[1], halfsize[2], 0)))

	local normal = QuatRotateVec(planeTransform.rot, { 0, 0, -1 })

	local rayDirDotNormal = VecDot(direction, normal)
	if (oneway and rayDirDotNormal or math.abs(rayDirDotNormal)) < 0 then
		-- Ray is parallel to plane, or wrong way; no intersection
		return { hit = false, normal = normal, dist = 1 / 0, dot = rayDirDotNormal }
	else
		local rayToPlane = VecSub(startPos, planeTransform.pos)
		local t = -VecDot(rayToPlane, normal) / rayDirDotNormal
		local intersection = VecAdd(startPos, VecScale(direction, t))

		local dist = AutoVecDist(startPos, intersection)

		-- Check if the intersection is inside the plane's bounds
		local edge1 = VecSub(corner2, corner1)
		local edge2 = VecSub(corner3, corner2)
		local edge3 = VecSub(corner4, corner3)
		local edge4 = VecSub(corner1, corner4)
		local vec1 = VecSub(intersection, corner1)
		local vec2 = VecSub(intersection, corner2)
		local vec3 = VecSub(intersection, corner3)
		local vec4 = VecSub(intersection, corner4)

		local isInside = true
		local function checkInsideEdge(vec, edge)
			if VecDot(edge, vec) < 0 then
				isInside = false
			end
		end

		checkInsideEdge(vec1, edge1)
		checkInsideEdge(vec2, edge2)
		checkInsideEdge(vec3, edge3)
		checkInsideEdge(vec4, edge4)

		return {
            hit = isInside and t > 0,
            intersection = intersection,
            normal = normal,
            dist = dist,
            dot = rayDirDotNormal,
		}
	end
end

---@return { hit:boolean, point:table, normal:table, shape:shape_handle, dist:number, dir:table, dot:number, reflection:table }
function AutoQueryClosest(origin, maxDist)
	local data = {}
	data.hit, data.point, data.normal, data.shape = QueryClosestPoint(origin, maxDist)

	if data.hit then
		local diff = VecSub(data.point, origin)
		local dir = VecNormalize(diff)
		local dot = VecDot(dir, data.normal)

		data.dist = VecLength(diff)
		data.dir = dir
		data.dot = dot
		data.reflection = VecSub(dir, VecScale(data.normal, 2 * dot))
	else
		data.dist = maxDist
	end

	return data
end

---@param shape shape_handle
---@return OBB
function AutoGetShapeOBB(shape)
	local transform = GetShapeWorldTransform(shape)
	local x, y, z, scale = GetShapeSize(shape)
	local size = VecScale(Vec(x, y, z), scale)

	local center = TransformToParentPoint(transform, VecScale(size, 0.5))
	return AutoOBB(center, transform.rot, size)
end

---Goes through each shape on a body and adds up their voxel count
---@param body any
---@return number
function AutoGetBodyVoxels(body)
	local v = 0
	for _, s in pairs(GetBodyShapes(body)) do
		v = v + GetShapeVoxelCount(s)
	end
	return v
end

function AutoScaleBodyVelocity(body, scale)
    local current = GetBodyVelocity(body)
    local scaled = VecScale(current, scale)
	SetBodyVelocity(body, scaled)
	return scaled, current
end

function AutoScaleBodyAngularVelocity(body, scale)
	local current = GetBodyAngularVelocity(body)
	local scaled = VecScale(current, scale)
	SetBodyAngularVelocity(body, scaled)
	return scaled, current
end

---Gets the angle from a point to the forward direction of a transform
---@param point any
---@param fromtrans any
---@return number
function AutoPointToAngle(point, fromtrans)
	fromtrans = AutoDefault(fromtrans, GetCameraTransform())

	local fromtopointdir = VecNormalize(VecSub(point, fromtrans.pos))
	local fromdir = AutoTransformFwd(fromtrans)

	local dot = VecDot(fromtopointdir, fromdir)
	return math.deg(math.acos(dot))
end

---Checks if a point is in the view using a transform acting as the "Camera"
---@param point vector
---@param oftrans Transfrom|nil The Transform acting as the camera, Default is the Player's Camera
---@param angle number|nil The Angle at which the point can be seen from, Default is the Player's FOV set in the options menu
---@param raycastcheck boolean|nil Check to make sure that the point is not obscured, Default is true
---@return boolean seen If the point is in View
---@return number|nil angle The Angle the point is away from the center of the looking direction
---@return number|nil distance The Distance from the point to fromtrans
function AutoPointInView(point, oftrans, angle, raycastcheck, raycasterror)
	oftrans = AutoDefault(oftrans, GetCameraTransform())
	angle = AutoDefault(angle, GetInt('options.gfx.fov'))
	raycastcheck = AutoDefault(raycastcheck, true)
	raycasterror = AutoDefault(raycasterror, 0)

	local useangle = not (angle < 0)

	local fromtopointdir = VecNormalize(VecSub(point, oftrans.pos))
	local fromdir = AutoTransformFwd(oftrans)

	local dist = AutoVecDist(oftrans.pos, point)

	local dot = VecDot(fromtopointdir, fromdir)
	local dotangle = math.deg(math.acos(dot))
	local seen = dotangle < angle / 2

	seen = (not useangle) and (true) or (seen)

	if seen then
		if raycastcheck then
			local hit, hitdist = QueryRaycast(oftrans.pos, fromtopointdir, dist, 0, true)
			if hit then
				if raycasterror > 0 then
					local hitpoint = VecAdd(oftrans.pos, VecScale(fromtopointdir, hitdist))
					if AutoVecDist(hitpoint, point) > raycasterror then
						seen = false
					end
				else
					seen = false
				end
			end
		end
	end

	return seen, dotangle, dist
end

function AutoPlayerInputDir(length)
	return VecScale({
		(InputDown('left') and -1 or 0) + (InputDown('right') and 1 or 0),
		0,
		(InputDown('down') and 1 or 0) + (InputDown('up') and -1 or 0),
	}, length or 1)
end

---Get the last Path Query as a path of points
---@param precision number The Accuracy
---@return table
---@return vector "Last Point"
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

---Reject a table of bodies for the next Query
---@param bodies table
function AutoQueryRejectBodies(bodies)
	for _, h in pairs(bodies) do
		if h then
			QueryRejectBody(h)
		end
	end
end

---Reject a table of shapes for the next Query
---@param shapes table
function AutoQueryRejectShapes(shapes)
	for _, h in pairs(shapes) do
		if h then
			QueryRejectShape(h)
		end
	end
end

function AutoRejectShapesWithoutTag(tag)
	local all = FindShapes(nil, true)
	local keep = {}
	for _, v in pairs(FindShapes(tag, true)) do
		keep[v] = v
	end

	for _, v in pairs(all) do
		if keep[v] == nil then QueryRejectShape(v) end
	end
end

---Set the collision filter for the shapes owned by a body
---@param body number
---@param layer number
---@param masknummber number bitmask
function AutoSetBodyCollisionFilter(body, layer, masknummber)
	local shapes = GetBodyShapes(body)
	for i in pairs(shapes) do
		SetShapeCollisionFilter(shapes[i], layer, masknummber)
	end
end

---Get the Center of Mass of a body in World space
---@param body any
---@return table
function AutoWorldCenterOfMass(body)
	local trans = GetBodyTransform(body)
	local pos = TransformToParentPoint(trans, GetBodyCenterOfMass(body))
	return pos
end

---Get the Total Speed of a body
---@param body body_handle
---@return number
function AutoSpeed(body)
	return VecLength(GetBodyVelocity(body)) + VecLength(GetBodyAngularVelocity(body))
end

---Attempt to predict the position of a body in time
---@param body number
---@param time number
---@param raycast boolean|nil Check and Halt on Collision, Default is false
---@param funcbefore function|nil
---@return table log
---@return table vel
---@return table normal
function AutoPredictPosition(body, time, raycast, funcbefore)
	raycast = AutoDefault(raycast, false)
	local point = {
		pos = AutoWorldCenterOfMass(body),
		vel = GetBodyVelocity(body)
	}
	local log = { VecCopy(point.pos) }
	local normal = Vec(0, 1, 0)

	for steps = 0, time, GetTimeStep() do
		if type(funcbefore) == "function" then
			funcbefore(point)
		elseif type(funcbefore) == "table" then
			for _, func in ipairs(funcbefore) do
				func(point)
			end
		end

		point.vel = VecAdd(point.vel, VecScale(Vec(0, -10, 0), GetTimeStep()))
		point.pos = VecAdd(point.pos, VecScale(point.vel, GetTimeStep()))
		log[#log + 1] = AutoTableDeepCopy(point.pos)

		if raycast then
			local last = log[#log - 1]
			local cur = log[#log]
			local hit, dist, norm = QueryRaycast(last, QuatLookAt(last, cur), VecLength(VecSub(cur, last)))
			if hit then normal = norm break end
		end
	end

	return log, point.vel, normal
end

---Attempt to predict the position of the player in time
---@param time number
---@param raycast boolean|nil Check and Halt on Collision, Default is false
---@return table log
---@return table vel
---@return table normal
function AutoPredictPlayerPosition(time, raycast)
	raycast = AutoDefault(raycast, false)
	local player = GetPlayerTransform(true)
	local pos = player.pos
	local vel = GetPlayerVelocity()
	local log = { VecCopy(pos) }
	local normal = Vec(0, 1, 0)

	for steps = 0, time, GetTimeStep() do
		vel = VecAdd(vel, VecScale(Vec(0, -10, 0), GetTimeStep()))
		log[#log + 1] = VecAdd(log[#log], VecScale(vel, GetTimeStep()))

		if raycast then
			local last = log[#log - 1]
			local cur = log[#log]
			local hit, dist, norm = QueryRaycast(last, QuatLookAt(last, cur), VecLength(VecSub(cur, last)))
			normal = norm

			if hit then break end
		end
	end

	return log, vel, normal
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Environment----------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Returns a table of every property of the current environment
---@return table
function AutoGetEnvironment()
	local params = {
		'skybox',
		'skyboxtint',
		'skyboxbrightness',
		'skyboxrot',
		'constant',
		'ambient',
		'ambientexponent',
		'fogColor',
		'fogParams',
		'sunBrightness',
		'sunColorTint',
		'sunDir',
		'sunSpread',
		'sunLength',
		'sunFogScale',
		'sunGlare',
		'exposure',
		'brightness',
		'wetness',
		'puddleamount',
		'puddlesize',
		'rain',
		'nightlight',
		'ambience',
		'fogscale',
		'slippery',
		'waterhurt',
		'snowdir',
		'snowamount',
		'snowonground',
		'wind',
	}

	local assembled = {}
	for _, k in ipairs(params) do
		assembled[k] = { GetEnvironmentProperty(k) }
	end

	return assembled
end

---Sets every environment property of AutoGetEnvironment
---@param Environment table
function AutoSetEnvironment(Environment)
	for k, v in pairs(Environment) do
		if type(v) == "table" then
			SetEnvironmentProperty(k, unpack(v))
		elseif type(v) == "function" then
			SetEnvironmentProperty(k, v())
		else
			SetEnvironmentProperty(k, v)
		end
	end
end

---comment
---@param r any
---@param g any
---@param b any
---@param a any
---@param sprite any|nil Defaults to TD's 'ui/menu/white-32.png'
function AutoSolidBackground(r, g, b, a, sprite, distance)
	r = AutoDefault(r, 0)
	g = AutoDefault(g, 0)
	b = AutoDefault(b, 0)
	a = AutoDefault(a, 1)
	sprite = AutoDefault(sprite, AutoFlatSprite)

	local cam = GetCameraTransform()
	local distance = distance or 256
	local overextention = 2
	local transforms = {
		Transform(VecAdd(cam.pos, Vec(0, 0, distance))),
		Transform(VecAdd(cam.pos, Vec(0, 0, -distance))),
		Transform(VecAdd(cam.pos, Vec(-distance, 0, 0)), QuatEuler(0, 90)),
		Transform(VecAdd(cam.pos, Vec(distance, 0, 0)), QuatEuler(0, 90)),
		Transform(VecAdd(cam.pos, Vec(0, -distance, 0)), QuatEuler(90, 0, 0)),
		Transform(VecAdd(cam.pos, Vec(0, distance, 0)), QuatEuler(90, 0, 0)),
	}
	for i, v in ipairs(transforms) do
		DrawSprite(sprite, v, distance * 2 + overextention, distance * 2 + overextention,
			r, g, b, a, true, false
		)
	end
end

function AutoSolidEnvironment(pathToDDS)
	return {
		skybox = { pathToDDS },
		wind = { 0, 0, 0 },
		sunFogScale = { 0 },
		puddleamount = { 0 },
		skyboxbrightness = { 1 },
		wetness = { 0 },
		snowonground = {},
		sunDir = { 0, 0, 0 },
		nightlight = { true },
		fogscale = { 0 },
		constant = { 0, 0, 0 },
		slippery = { 0 },
		brightness = { 1 },
		snowdir = { 0, 0, 1, 0 },
		snowamount = { 0, 0 },
		skyboxtint = { 1, 1, 1 },
		sunLength = { 0 },
		sunGlare = { 0 },
		waterhurt = { 0 },
		fogColor = { 0, 0, 0 },
		exposure = { 1, 1 },
		ambience = { "outdoor/field.ogg", 0 },
		ambientexponent = { 10 ^ -37.9275 },
		fogParams = { 0, 0, 0, 0 },
		sunBrightness = { 0 },
		ambient = { 1 },
		skyboxrot = { 0 },
		puddlesize = { 0 },
		rain = { 0 },
		sunColorTint = { 0, 0, 0 },
		sunSpread = { 0 },
	}
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Post Processing----------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Returns a table of every property of the current post-processing
---@return table
function AutoGetPostProcessing()
	local params = {
		'saturation',
		'colorbalance',
		'brightness',
		'gamma',
		'bloom',
	}

	local assembled = {}
	for _, k in ipairs(params) do
		assembled[k] = { GetPostProcessingProperty(k) }
	end

	return assembled
end

---Sets every post-processing property of AutoGetPostProcessing
---@param PostProcessing table
function AutoSetPostProcessing(PostProcessing)
	for k, v in pairs(PostProcessing) do
		if type(v) == "table" then
			SetPostProcessingProperty(k, unpack(v))
		elseif type(v) == "function" then
			SetPostProcessingProperty(k, v())
		else
			SetPostProcessingProperty(k, v)
		end
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Showing Debug--------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

--- Returns the current Line Number.
---
--- This function is adapted from the UMF Framework
---
--- https://github.com/Thomasims/TeardownUMF/blob/master/src/util/debug.lua
---@param level number|nil Optional
---@return number
function AutoGetCurrentLine(level)
	level = (level or 0)
	local _, line = pcall(error, '', level + 3) -- The level + 3 is to get out of error, then out of pcall, then out of this function
	return tonumber(AutoSplit(line, ':')[2])
end

--- Returns the current Line Number.
---
--- This function is adapted from the UMF Framework
---
--- https://github.com/Thomasims/TeardownUMF/blob/master/src/util/debug.lua
---@param level number|nil Optional
---@return string|nil
function AutoGetStackTrace(level)
	level = (level or 0)
	local _, line = pcall(error, '', level + 3) -- The level + 3 is to get out of error, then out of pcall, then out of this function
	return line
end

---Creates a neatly formatted table of a value, including tables.
---@param t any
---@param singleline_at number?
---@param indent_str string?
---@param round_numbers number|false?
---@param indents number?
---@param visited_tables table?
---@return string
function AutoToString(t, singleline_at, indent_str, round_numbers, format_keys, indents, visited_tables)
	local singleline_at = singleline_at or 1
	local indent_str = indent_str or '  '
	local round_numbers = round_numbers or false
	local format_keys = format_keys or false
	local indents = indents or 0
	local visited_tables = visited_tables or {}
	local str = ""

	if type(t) ~= "table" then
		if type(t) == "string" then
			return string.format("%q", t)
		elseif type(t) == 'number' then
			if round_numbers then
				return tostring(AutoRound(t, round_numbers))
			else
				return tostring(t)
			end
		end
		return tostring(t)
	else
		if visited_tables[t] then return string.format('"<circular reference of %s>"', tostring(t)) end
		visited_tables[t] = true
		if AutoTableCount(t) == 0 then return('{}') end
	end
	
	local passedSLthreshold = singleline_at <= 0
	str = passedSLthreshold and "{ " or "{\n"
	if passedSLthreshold then indents = indents + 1 end

	for k, v in pairs(t) do
		if not passedSLthreshold then
			str = str .. indent_str:rep(indents+1)
		end

		local k_str = type(k) == "number" and '' or ((format_keys and string.format("%q", tostring(k)) or tostring(k)) .. " = ")
		local v_str = AutoToString(v, singleline_at - 1, indent_str, round_numbers, format_keys, indents + 1, visited_tables)
		str = str .. k_str .. v_str .. ", "

		if not passedSLthreshold then
			str = str .. "\n"
		end
	end
	str = (passedSLthreshold and str:sub(1, -3) or str) .. (passedSLthreshold and " }" or (indent_str:rep(indents) .. "}"))
	return str
end

---A Alternative to DebugPrint that uses AutoInspect(), works with tables. Returns the value
---@param value any
---@param singleline_at any
---@param indent_str any
---@return any
function AutoInspect(value, singleline_at, indent_str, round_numbers)
	local text = AutoToString(value, singleline_at or 3, indent_str, round_numbers)
	local split = AutoSplit(text, '\n')
	for i=1, #split do
		local t = split[i]
		if i > 20 and i == #split then
			t = t .. ' - Some of AutoInspect has been cut off (' .. #split - 20 .. ' Lines Cut), lowering `singleline_at` may give more room'
		end
		DebugPrint(t)
	end
	
	return value
end

---AutoInspect that prints to console
---@param value any
---@param singleline_at any
---@param indent_str any
---@return any
function AutoInspectConsole(value, singleline_at, indent_str, round_numbers)
	print(AutoToString(value, singleline_at or 3, indent_str, round_numbers))
	return value
end

function AutoInspectWatch(value, name, singleline_at, indent_str, round_numbers)
	if not name then name = 'Inspecting Line ' .. AutoGetCurrentLine(1) end
	DebugWatch(name, AutoToString(value, singleline_at, indent_str, round_numbers))
end

---Prints 24 blank lines to quote on quote, "clear the console"
function AutoClearConsole()
	for i = 1, 24 do DebugPrint('') end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Drawing Stuff--------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Draws a table of
---@param points any
---@param huescale number|nil A multipler to the hue change, Default is 1
---@param offset number|nil Offsets the hue for every point, Default is 0
---@param alpha number|nil Sets the alpha of the line, Default is 1
---@param draw boolean|nil Whether to use DebugLine or DrawLine, Default is false (DebugLine)
function AutoDrawLines(points, huescale, offset, alpha, draw)
	huescale = AutoDefault(huescale, 1)
	offset = AutoDefault(offset, 0)
	alpha = AutoDefault(alpha, 1)
	draw = AutoDefault(draw, false)

	local lines = {}
	local dist = 0
	for i = 1, #points - 1 do
		local c1, c2, c3 = AutoHSVToRGB((dist / 10 * huescale) + offset, 0.5, 1)
		table.insert(lines, { points[i], points[i + 1], c1, c2, c3, alpha })

		dist = dist + AutoVecDist(points[i], points[i + 1])
	end

	for i, v in ipairs(lines) do
		if draw then
			DrawLine(unpack(v))
		else
			DebugLine(unpack(v))
		end
	end
end

---Draws a Transform
---@param transform transform|vector
---@param size number|nil the size in meters, Default is 0.5
---@param alpha number|nil Default is 1
---@param draw boolean|nil Whether to use DebugLine or DrawLine, Default is false (DebugLine)
function AutoDrawTransform(transform, size, alpha, hueshift, draw)
	if not transform['pos'] then
		-- DebugPrint('AutoDrawTransform given input not a transform')
		transform = Transform(transform)
	end

	transform.rot = AutoDefault(transform.rot, QuatEuler(0, 0, 0))
	size = AutoDefault(size, 0.5)
	alpha = AutoDefault(alpha, 1)
	draw = AutoDefault(draw, false)

	local right = TransformToParentPoint(transform, Vec(size, 0, 0))
	local up = TransformToParentPoint(transform, Vec(0, size, 0))
	local forward = TransformToParentPoint(transform, Vec(0, 0, size))

	local lines = {
		{ transform['pos'], right, 1, 0, 0, alpha },
		{ transform['pos'], up, 0, 1, 0, alpha },
		{ transform['pos'], forward, 0, 0, 1, alpha },
	}

	for i, v in ipairs(lines) do
		if draw then
			DrawLine(unpack(v))
		else
			DebugLine(unpack(v))
		end
	end

	return transform
end

function AutoDrawPlane(transform, size, pattern, patternstrength, oneway, r, g, b, a)
	-- Extract position and rotation from the Transform table
	local pos = transform.pos or Vec(0, 0, 0)
	local rot = transform.rot or Quat()

	-- Calculate the forward, right, and up vectors from the rotation
	local forward = QuatRotateVec(rot, Vec(0, 0, 1))
	local right = QuatRotateVec(rot, Vec(1, 0, 0))
	local up = QuatRotateVec(rot, Vec(0, 1, 0))

	-- Calculate the corners of the plane
	local corner1 = VecAdd(VecAdd(pos, VecScale(right, -size[1] / 2)), VecScale(up, -size[2] / 2))
	local corner2 = VecAdd(VecAdd(pos, VecScale(right, size[1] / 2)), VecScale(up, -size[2] / 2))
	local corner3 = VecAdd(VecAdd(pos, VecScale(right, size[1] / 2)), VecScale(up, size[2] / 2))
	local corner4 = VecAdd(VecAdd(pos, VecScale(right, -size[1] / 2)), VecScale(up, size[2] / 2))

	r, g, b, a = r or 1, g or 1, b or 1, a or 1

	if oneway and VecDot(VecSub(pos, GetCameraTransform().pos), forward) < 0 then
		return
	end
	
	-- Draw the grid
	if pattern == 0 then
		patternstrength = (patternstrength or 0) + 1
		for i = 0, patternstrength do
			local subH1 = VecLerp(corner1, corner2, i / patternstrength)
			local subH2 = VecLerp(corner4, corner3, i / patternstrength)
			local subV1 = VecLerp(corner1, corner4, i / patternstrength)
			local subV2 = VecLerp(corner2, corner3, i / patternstrength)
	
			DebugLine(subH1, subH2, r, g, b, a)
			DebugLine(subV1, subV2, r, g, b, a)
		end
	elseif pattern > 0 then
		DebugLine(corner1, corner2, r, g, b, a)
		DebugLine(corner2, corner3, r, g, b, a)
		DebugLine(corner3, corner4, r, g, b, a)
		DebugLine(corner4, corner1, r, g, b, a)
	end

	if pattern == 1 or pattern == 3 then
		patternstrength = (patternstrength or 1)
		local step = 1 / patternstrength
		for t = step, 2, step * 2 do
			local p1 = t <= 1 and VecLerp(corner1, corner2, t) or VecLerp(corner2, corner3, t - 1)
			local p2 = t <= 1 and VecLerp(corner1, corner4, t) or VecLerp(corner4, corner3, t - 1)

			DebugLine(p1, p2, r, g, b, a)
		end
	end
	
	if pattern == 2 or pattern == 3 then
		patternstrength = (patternstrength or 0)
		local step = 1 / patternstrength
		for t = step, 2, step * 2 do
			local p1 = t <= 1 and VecLerp(corner2, corner3, t) or VecLerp(corner3, corner4, t - 1)
			local p2 = t <= 1 and VecLerp(corner2, corner1, t) or VecLerp(corner1, corner4, t - 1)

			DebugLine(p1, p2, r, g, b, a)
		end

		DebugLine(corner1, corner2, r, g, b, a)
		DebugLine(corner2, corner3, r, g, b, a)
		DebugLine(corner3, corner4, r, g, b, a)
		DebugLine(corner4, corner1, r, g, b, a)
	end
end

function AutoDrawBox(point, halfextents, r, g, b, a)
	local aa, bb = AutoAABBExpandPoint(point, halfextents)
	AutoDrawAABB(aa, bb, r, g, b, a)
	
	return aa, bb
end

---Draws a Transform as a Cone
---@param transform transform
---@param sides number|nil the amount of sides on the cone, Default is 12
---@param angle number|nil how wide the cone is in degrees, Default is 25
---@param size number|nil the size in meters, Default is 0.5
---@param color table|nil Default is 1
---@param draw boolean|nil Whether to use DebugLine or DrawLine, Default is false (DebugLine)
function AutoDrawCone(transform, sides, angle, size, color, draw)
	if not transform['pos'] then
		DebugPrint('AutoDrawCone given input not a transform')
	end

	transform.rot = AutoDefault(transform.rot, QuatEuler(0, 0, 0))
	size = AutoDefault(size, 0.5)
	color = AutoDefault(color, { 1, 1, 1, 1 })
	draw = AutoDefault(draw, false)
	sides = AutoDefault(sides, 12)
	angle = AutoDefault(angle, 25)

	local forward = TransformToParentPoint(transform, Vec(0, 0, size))

	local lines = {}
	local endpoints = {}
	for i = 1, sides do
		local rotated = TransformCopy(transform)
		local rr = i / sides * 360
		rotated.rot = QuatRotateQuat(rotated.rot, QuatEuler(angle, 0, rr))

		local endpoint = TransformToParentPoint(rotated, Vec(0, 0, -size))
		lines[#lines + 1] = { transform['pos'], endpoint }
		endpoints[#endpoints + 1] = endpoint
	end

	for i = 1, #endpoints do
		lines[#lines + 1] = { endpoints[i], endpoints[AutoWrap(i + 1, 1, #endpoints)] }
	end

	for i, v in ipairs(lines) do
		if draw then
			DrawLine(v[1], v[2], unpack(color))
		else
			DebugLine(v[1], v[2], unpack(color))
		end
	end

	return transform
end

---Draws some text at a world position.
---@param text string|number|nil Text Displayed, Default is 'nil'
---@param position vector The WorldSpace Position
---@param occlude boolean|nil Hides the tooltip behind walls, Default is false

---@param fontsize number|nil Fontsize, Default is 24
---@param alpha number|nil Alpha, Default is 0.75
---@param bold boolean|nil Use Bold Font, Default is false
function AutoTooltip(text, position, occlude, fontsize, alpha)
	text = AutoDefault(text or "nil")
	occlude = AutoDefault(occlude or false)
	fontsize = AutoDefault(fontsize or 24)
	alpha = AutoDefault(alpha or 0.75)
	bold = AutoDefault(bold or false)

	if occlude then if not AutoPointInView(position, nil, nil, occlude) then return end end

	UiPush()
	UiAlign('center middle')
	local x, y, dist = UiWorldToPixel(position)
	if dist > 0 then
		UiTranslate(x, y)
		UiWordWrap(UiMiddle())
		
		UiFont(AutoFont, fontsize)
		UiColor(0, 0, 0, 0)
		local rw, rh = UiText(text)
		
		UiColorFilter(1, 1, 1, alpha)
		UiColor(unpack(AutoSecondaryColor))
		UiRect(rw, rh)
		
		UiColor(unpack(AutoPrimaryColor))
		UiText(text)
		UiPop()
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Graphing-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

AutoGraphs = {}

---Creates a Continuous Graph that can be drawn. The given value is added into the graph as the previous ones are kept in memory.
---@param id string
---@param value number
---@param range number|nil Default is 64
function AutoGraphContinuous(id, value, range)
	local Graph = AutoDefault(AutoGraphs[id], {
		scan = 0,
		range = AutoDefault(range, 64),
		values = {}
	})

	Graph.scan = AutoWrap(Graph.scan + 1, 1, range)
	Graph.values[Graph.scan] = value

	AutoGraphs[id] = Graph
end

---Creates a Graph with values within a range fed into a given function.
---@param id string
---@param rangemin number|nil Default is 0
---@param rangemax number|nil Default is 1
---@param func function|nil Is fed one parameter, a number ranging from rangemin to rangemax, Defaults to a Logisitc Function
---@param steps number|nil How many steps, or the interval of values taken from the range.
function AutoGraphFunction(id, rangemin, rangemax, func, steps)
	rangemin = AutoDefault(rangemin, 0)
	rangemax = AutoDefault(rangemax, 1)
	steps = AutoDefault(steps, 100)

	func = AutoDefault(func, function(x)
		return AutoLogistic(x, 1, -10, 0.5)
	end)

	local Graph = {
		values = {}
	}

	for i = 1, steps do
		local v = func(AutoMap(i / steps, 0, 1, rangemin, rangemax))
		Graph.values[i] = v
	end

	AutoGraphs[id] = Graph
end

---Draws a given graph with some parameters
---@param id string
---@param sizex number width of the graph, Default is 128
---@param sizey number height of the graph, Default is 64
---@param rangemin number|nil If left nil, then the graph will automatically stretch the values to fill the bottom of the graph. Default is nil
---@param rangemax number|nil If left nil, then the graph will automatically stretch the values to fill the top of the graph. Default is nil
---@param linewidth number|nil The line width, Default is 2
function AutoGraphDraw(id, sizex, sizey, rangemin, rangemax, linewidth)
	local Graph = AutoGraphs[id]
	if Graph == nil then error("Graph Doesn't exist, nil") end

	sizex = AutoDefault(sizex, 128)
	sizey = AutoDefault(sizey, 64)

	UiPush()
	UiWindow(sizex + AutoPad.micro * 2, sizey + AutoPad.micro * 2, true)

	local minval = 0
	local maxval = 0
	for _, v in ipairs(Graph.values) do
		if v < minval then minval = v end
		if v > maxval then maxval = v end
	end

	minval = AutoDefault(rangemin, minval)
	maxval = AutoDefault(rangemax, maxval)

	for i = 1, #Graph.values - 1 do
		if Graph.values[i] == AutoClamp(Graph.values[i], minval, maxval) then
			local a = Vec(
				AutoMap(i, 1, #Graph.values, 0, sizex),
				AutoMap(Graph.values[i], minval, maxval, sizey, 0),
				0
			)
			local b = Vec(
				AutoMap(i + 1, 1, #Graph.values, 0, sizex),
				AutoMap(Graph.values[i + 1], minval, maxval, sizey, 0),
				0
			)

			local angle = math.atan2(b[1] - a[1], b[2] - a[2]) * 180 / math.pi
			local distance = AutoVecDist(a, b)
			local width = AutoDefault(linewidth, 2)

			UiPush()
			UiTranslate(a[1] - width / 2, a[2] - width / 2)
			UiRotate(angle)

			UiColor(unpack(AutoPrimaryColor))
			UiAlign('left top')
			UiRect(width, distance + 1)
			UiPop()
		end
	end
	UiPop()

	local data = { rect = { w = sizex + AutoPad.micro * 2, h = sizey + AutoPad.micro * 2 } }
	AutoHandleSpread(AutoGetSpread(), data, 'draw')
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Registry-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

function AutoKey(...)
	local s = ''
	for i = 1, #arg do
		if s == '' then
			s = arg[i]
		else
			s = s .. '.' .. arg[i]
		end
	end
	return s
end

function AutoExpandRegistryKey(key)
	local t = {}
	local function delve(k, current)
		local subkeys = ListKeys(k)
		local splitkey = AutoSplit(k, '.')
		local neatkey = splitkey[#splitkey]
		if #subkeys > 0 then
			current[neatkey] = {}
			for _, subkey in ipairs(subkeys) do
				delve(AutoKey(k, subkey), current[neatkey])
			end
		else
			current[neatkey] = HasKey(k) and GetString(k) or nil
		end
	end
	
	for _, k in pairs(ListKeys(key)) do
		delve(AutoKey(key, k), t)
	end
	
	return t
end

function delve(key, current)
	local subkeys = ListKeys(key)
	local splitkey = AutoSplit(key, '.')
	local neatkey = splitkey[#splitkey]
	if #subkeys > 0 then
		current[neatkey] = {}

		for _, subkey in ipairs(subkeys) do
			delve(AutoKey(key, subkey), current[neatkey])
		end
	else
		current[neatkey] = HasKey(key) and GetString(key) or nil
	end
end

function AutoKeyDefaultInt(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetInt(path, default)
	else
		SetInt(path, default)
		return default
	end
end

function AutoKeyDefaultFloat(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetFloat(path, default)
	else
		SetFloat(path, default)
		return default
	end
end

function AutoKeyDefaultString(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetString(path, default)
	else
		SetString(path, default)
		return default
	end
end

function AutoKeyDefaultBool(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetBool(path)
	else
		SetBool(path, default)
		return default
	end
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Registry Binded Table------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

local RegistryTableMeta = {
	__index = function(self, key)
		key = key:lower()
		local path = AutoKey(rawget(self, '__path'), key)
		if not HasKey(path) then
			return nil
		end

		local type = GetString(AutoKey(path, '__type'))

		if type == 'table' then
			return AutoRegistryBindedTable(path)
		else
			local str = GetString(path)

			if type == 'number' then
				return tonumber(str)
			end

			return str
		end
	end,
	__newindex = function(self, key, value)
		key = key:lower()
		local path = AutoKey(rawget(self, '__path'), key)

		local function dive(p, v)
			if type(v) ~= "table" then
				SetString(p, v)

				if type(v) ~= "nil" then
					SetString(AutoKey(p, '__type'), type(v))
				end
			else
				SetString(AutoKey(p, '__type'), 'table')
				for k, set in pairs(v) do
					dive(AutoKey(p, k), set)
				end
			end
		end

		dive(path, value)
	end,
	__call = function(self)
		local path = rawget(self, '__path')

		local function dive(p)
			local keys = ListKeys(p)
			local full = {}

			for i = 1, #keys do
				local child = AutoKey(p, keys[i])

				if keys[i] ~= '__type' then
					local t = GetString(AutoKey(child, '__type'))
					if t == 'table' then
						full[keys[i]] = dive(child)
					else
						local str = GetString(child)
						local num = tonumber(str)
						full[keys[i]] = num or str
					end
				end
			end

			return full
		end

		return dive(path)
	end
}

function AutoRegistryBindedTable(path)
	local t = {}
	t.__path = path
	setmetatable(t, RegistryTableMeta)

	return t
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

AutoPad = { none = 0, atom = 4, micro = 6, thin = 12, thick = 24, heavy = 48, beefy = 128 }

AutoPrimaryColor = { 0.95, 0.95, 0.95, 1 }
AutoSpecialColor = { 1, 1, 0.55, 1 }
AutoSecondaryColor = { 0, 0, 0, 0.55 }
AutoFont = 'regular.ttf'
local SpreadStack = {}

---Takes an alignment and returns a Vector representation.
---@param alignment string
---@return table
function AutoAlignmentToPos(alignment)
	str, y = 0, 0
	if string.find(alignment, 'left') then str = -1 end
	if string.find(alignment, 'center') then str = 0 end
	if string.find(alignment, 'right') then str = 1 end
	if string.find(alignment, 'bottom') then y = -1 end
	if string.find(alignment, 'middle') then y = 0 end
	if string.find(alignment, 'top') then y = 1 end
	return { x = str, y = y }
end

---UiTranslate and UiAlign to the Center
function AutoCenter()
	UiTranslate(UiCenter(), UiMiddle())
	UiAlign('center middle')
end

function AutoUiBounds(subtract)
	subtract = subtract or 0
	return UiWidth() - subtract, UiHeight() - subtract
end

---The next Auto Ui functions will be spread Down until AutoSpreadEnd() is called
---@param padding number|nil The amount of padding that will be used, Default is AutoPad.thin
function AutoSpreadDown(padding)
	table.insert(SpreadStack, { type = 'spread', direction = 'down', padding = AutoDefault(padding, AutoPad.thin) })
	UiPush()
end

---The next Auto Ui functions will be spread Up until AutoSpreadEnd() is called
---@param padding number|nil The amount of padding that will be used, Default is AutoPad.thin
function AutoSpreadUp(padding)
	table.insert(SpreadStack, { type = 'spread', direction = 'up', padding = AutoDefault(padding, AutoPad.thin) })
	UiPush()
end

---The next Auto Ui functions will be spread Right until AutoSpreadEnd() is called
---@param padding number|nil The amount of padding that will be used, Default is AutoPad.thin
function AutoSpreadRight(padding)
	table.insert(SpreadStack, { type = 'spread', direction = 'right', padding = AutoDefault(padding, AutoPad.thin) })
	UiPush()
end

---The next Auto Ui functions will be spread Left until AutoSpreadEnd() is called
---@param padding number|nil The amount of padding that will be used, Default is AutoPad.thin
function AutoSpreadLeft(padding)
	table.insert(SpreadStack, { type = 'spread', direction = 'left', padding = AutoDefault(padding, AutoPad.thin) })
	UiPush()
end

---The next Auto Ui functions will be spread Verticlely across the Height of the Bounds until AutoSpreadEnd() is called
---@param count number|nil The amount of Auto Ui functions until AutoSpreadEnd()
function AutoSpreadVerticle(count)
	table.insert(SpreadStack, { type = 'spread', direction = 'verticle', length = UiHeight(), count = count })
	UiPush()
end

---The next Auto Ui functions will be spread Horizontally across the Width of the Bounds until AutoSpreadEnd() is called
---@param count number|nil The amount of Auto Ui functions until AutoSpreadEnd()
function AutoSpreadHorizontal(count)
	table.insert(SpreadStack, { type = 'spread', direction = 'horizontal', length = UiWidth(), count = count })
	UiPush()
end

function AutoGetSpread()
	local _l = 0
	local count = AutoTableCount(SpreadStack)
	if count <= 0 then return nil end
	for i = count, 1, -1 do
		if SpreadStack[i].type == 'spread' then
			_l = _l + 1
			if _l >= 1 then
				return SpreadStack[i], _l
			end
		end
	end
	return nil
end

function AutoSetSpread(Spread)
	local count = AutoTableCount(SpreadStack)
	for i = count, 1, -1 do
		if SpreadStack[i].type == 'spread' then
			str = SpreadStack[i]
		end
	end

	str = Spread
end

---Stop the last known Spread
---@return table a table with information about the transformations used
function AutoSpreadEnd()
	local unitdata = { comb = { w = 0, h = 0 }, max = { w = 0, h = 0 } }
	-- local _, LastSpread = AutoGetSpread(1)

	while true do
		local count = #SpreadStack

		if SpreadStack[count].type ~= 'spread' then
			if SpreadStack[count].data.rect then
				local rect = SpreadStack[count].data.rect
				unitdata.comb.w, unitdata.comb.h = unitdata.comb.w + rect.w, unitdata.comb.h + rect.h
				unitdata.max.w, unitdata.max.h = math.max(unitdata.max.w, rect.w), math.max(unitdata.max.h, rect.h)
			end

			table.remove(SpreadStack, count)
		else
			UiPop()
			table.remove(SpreadStack, count)

			return unitdata
		end
		if count <= 0 then
			return unitdata
		end
	end
end

function AutoHandleSpread(gs, data, type, spreadpad)
	spreadpad = AutoDefault(spreadpad, false)

	if not AutoGetSpread() then return end

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
		table.insert(SpreadStack, { type = type, data = data })
	end
end

---Given the current string, will return a modified string based on the input of the user. It's basically just a text box. Has a few options.
---@param current any
---@param maxlength any
---@param allowlowercase any
---@param allowspecial any
---@param forcekey any
---@return any
---@return any
---@return boolean
function AutoTextInput(current, maxlength, allowlowercase, allowspecial, forcekey)
	current = AutoDefault(current, '')
	maxlength = AutoDefault(maxlength, 1 / 0)
	allowlowercase = AutoDefault(allowlowercase, true)
	allowspecial = AutoDefault(allowspecial, true)
	forcekey = AutoDefault(forcekey, nil)

	local modified = current

	local special = {
		['1'] = '!',
		['2'] = '@',
		['3'] = '#',
		['4'] = '$',
		['5'] = '%',
		['6'] = '^',
		['7'] = '&',
		['8'] = '*',
		['9'] = '(',
		['0'] = ')',
	}
	local lpk = forcekey or InputLastPressedKey()

	if lpk == 'backspace' then
		modified = modified:sub(1, #modified - 1)
	elseif lpk == 'delete' then
		modified = ''
	elseif #modified < maxlength then
		if lpk == 'space' then
			modified = modified .. ' '
		elseif #lpk == 1 then
			if not InputDown('shift') then
				if allowlowercase then
					lpk = lpk:lower()
				end
			else
				if allowspecial and special[lpk] then
					lpk = special[lpk]
				end
			end

			modified = modified .. lpk
		end
	end

	return modified, lpk ~= '' and lpk or nil, modified ~= current
end

-- local keys = {
-- 	"lmb", "mmb", "rmb", -- mouse
-- 	"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", -- numerical
-- 	"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
-- 	"y", "z", -- alphabatical
-- 	"f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12", -- function key
-- 	"uparrow", "downarrow", "leftarrow", "rightarrow", -- arrow key
-- 	"backspace", "alt", "delete", "home", "end", "pgup", "pgdown", "insert", "return", "space", "shift", "ctrl", "tab",
-- 	"esc", --random key
-- 	",", ".", "-", "+", -- undocumented key (yes, '=' key is '+' key)
-- }

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface Creation Functions------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Create a Container with new bounds
---@param width number
---@param height number
---@param padding number|nil The Amount of padding against sides of the container, Default is AutoPad.micro
---@param clip boolean|nil Whether  to clip stuff outside of the container, Default is false
---@param draw boolean|nil Draws the container's background, otherwise it will be invisible, Default is true
---@return table containerdata
function AutoContainer(width, height, padding, clip, draw)
	width = AutoDefault(width, 300)
	height = AutoDefault(height, 400)
	padding = math.max(AutoDefault(padding, AutoPad.micro), 0)
	clip = AutoDefault(clip, false)
	draw = AutoDefault(draw, true)

	local paddingwidth = math.max(width - padding * 2, padding * 2)
	local paddingheight = math.max(height - padding * 2, padding * 2)

	UiWindow(width, height, clip)

	UiAlign('left top')
	if draw then
		UiPush()
		UiColor(unpack(AutoSecondaryColor))
		UiImageBox("ui/common/box-solid-10.png", UiWidth(), UiHeight(), 10, 10)
		UiPop()
	end

	hover = UiIsMouseInRect(UiWidth(), UiHeight())

	UiTranslate(padding, padding)
	UiWindow(paddingwidth, paddingheight, false)

	local offset = { x = 0, y = 0 }

	UiTranslate(offset.x, offset.y)

	return { rect = { w = paddingwidth, h = paddingheight }, hover = hover }
end

---Creates a Button
---@param name string
---@param fontsize number
---@param paddingwidth number Amount of padding used Horizontally
---@param paddingheight number Amount of padding used Vertically
---@param draw boolean Draws the Button
---@param spreadpad boolean Adds padding when used with AutoSpread...()
---@return boolean Pressed
---@return table ButtonData
function AutoButton(name, fontsize, color, paddingwidth, paddingheight, draw, spreadpad)
	fontsize = AutoDefault(fontsize, 28)
	color = AutoDefault(color, AutoPrimaryColor)
	paddingwidth = AutoDefault(paddingwidth, AutoPad.thick)
	paddingheight = AutoDefault(paddingheight, AutoPad.thin)
	draw = AutoDefault(draw, true)
	spreadpad = AutoDefault(spreadpad, true)

	UiPush()
	UiWordWrap(UiWidth() - AutoPad.thick)
	UiFont(AutoFont, fontsize)
	UiButtonHoverColor(unpack(AutoSpecialColor))
	UiButtonPressColor(0.75, 0.75, 0.75, 1)
	UiButtonPressDist(0.25)

	UiColor(0, 0, 0, 0)
	local rw, rh = UiText(name)
	local padrw, padrh = rw + paddingwidth * 2, rh + paddingheight * 2

	if draw then
		hover = UiIsMouseInRect(padrw, padrh)
		UiColor(unpack(color))

		UiButtonImageBox('ui/common/box-outline-6.png', 6, 6, unpack(color))
		pressed = UiTextButton(name, padrw, padrh)
	end
	UiPop()

	local data = { pressed = pressed, hover = hover, rect = { w = padrw, h = padrh } }
	if draw then AutoHandleSpread(AutoGetSpread(), data, 'draw', spreadpad) end

	return pressed, data
end

---Draws some Text
---@param name string
---@param fontsize number
---@param draw boolean Draws the Text
---@param spread boolean Adds padding when used with AutoSpread...()
---@return table TextData
function AutoText(name, fontsize, color, draw, spread)
	fontsize = AutoDefault(fontsize, 28)
	draw = AutoDefault(draw, true)
	spread = AutoDefault(spread, true)

	UiPush()
	UiWordWrap(UiWidth() - AutoPad.thick)
	UiFont(AutoFont, fontsize)

	UiColor(0, 0, 0, 0)
	local rw, rh = UiText(name)

	if draw then
		UiPush()
		UiWindow(rw, rh)
		AutoCenter()

		UiColor(unpack(color or AutoPrimaryColor))
		UiText(name)
		UiPop()
	end
	UiPop()

	local data = { rect = { w = rw, h = rh }, hover = UiIsMouseInRect(rw, rh) }
	if spread then AutoHandleSpread(AutoGetSpread(), data, 'draw', true) end

	return data
end

---Creates a Slider
---@param set number The Current Value
---@param min number The Minimum
---@param max number The Maximum
---@param lockincrement number The increment
---@param paddingwidth Amount of padding used Horizontally
---@param paddingheight Amount of padding used Vertically
---@param spreadpad boolean Adds padding when used with AutoSpread...()
---@return number NewValue
---@return table SliderData
function AutoSlider(set, min, max, lockincrement, paddingwidth, paddingheight, spreadpad)
	min = AutoDefault(min, 0)
	max = AutoDefault(max, 1)
	set = AutoDefault(set, min)
	lockincrement = AutoDefault(lockincrement, 0)
	paddingwidth = AutoDefault(paddingwidth, AutoPad.thick)
	paddingheight = AutoDefault(paddingheight, AutoPad.micro)
	spreadpad = AutoDefault(spreadpad, true)

	local width = UiWidth() - paddingwidth * 2
	local dotwidth, dotheight = UiGetImageSize("MOD/slider.png")

	local screen = AutoMap(set, min, max, 0, width)

	UiPush()
	UiTranslate(paddingwidth, paddingheight)
	UiColor(unpack(AutoSpecialColor))

	UiPush()
	UiTranslate(0, dotheight / 2)
	UiRect(width, 2)
	UiPop()

	UiTranslate(-dotwidth / 2, 0)

	screen, released = UiSlider('MOD/slider.png', "x", screen, 0, width)
	screen = AutoMap(screen, 0, width, min, max)
	screen = AutoRound(screen, lockincrement)
	screen = AutoClamp(screen, min, max)
	set = screen
	UiPop()

	local data = { value = set, released = released, rect = { w = width, h = paddingheight * 2 + dotheight } }
	AutoHandleSpread(AutoGetSpread(), data, 'draw', spreadpad)

	return set, data
end

---Draws an Image
---@param path string
---@param width number
---@param height number
---@param alpha number
---@param draw boolean Draws the Image
---@param spreadpad boolean Adds padding when used with AutoSpread...()
---@return table ImageData
function AutoImage(path, width, height, border, spreadpad)
	local w, h = UiGetImageSize(path)
	width = AutoDefault(width, (height == nil and UiWidth() or (height * (w / h))))
	height = AutoDefault(height, width * (h / w))
	border = AutoDefault(border, 0)
	draw = AutoDefault(draw, true)
	spreadpad = AutoDefault(spreadpad, true)

	UiPush()
	UiImageBox(path, width, height, border, border)
	UiPop()

	local hover = UiIsMouseInRect(width, height)

	local data = { hover = hover, rect = { w = width, h = height } }
	if draw then AutoHandleSpread(AutoGetSpread(), data, 'draw', spreadpad) end

	return data
end

function AutoUiLine(p1, p2, width)
	width = AutoDefault(width, 2)
	local angle = math.atan2(p2[1] - p1[1], p2[2] - p1[2]) * 180 / math.pi
	local distance = AutoVecDist(p1, p2)

	UiPush()
		UiTranslate(p1[1] - width / 2, p1[2] - width / 2)
		UiRotate(angle)

		UiAlign('center top')
		UiRect(width, distance + 1)
	UiPop()
end

---Creates a handy little marker, doesnt effect anything, purely visual
---@param size number, Default is 1
function AutoMarker(size)
	size = AutoDefault(size, 1) / 2
	UiPush()
	UiAlign('center middle')
	UiScale(size, size)
	UiColor(unpack(AutoSpecialColor))
	UiImage('ui/common/dot.png')
	UiPop()
end