--[[
	VERSION 5.0 - This version scheme is essentially meaningless, I change it all the time.
	I ask that you do not rename Automatic.lua - Thank you
	Documentation assumes that you are using Teardown Totally Documented's lua library for type annotations.
]]

--#region Shortcuts

AutoFlatSprite = LoadSprite('ui/menu/white_32.png')
AutoPalette = {
	vfchill = {
		background_dark =  { 0.28627450980392, 0.25490196078431,  0.38039215686275 },
		background_light = { 0.41960784313725, 0.39607843137255,  0.58823529411765 },
		wood_dark =        { 0.6,              0.33725490196078,  0.42352941176471 },
		wood_light =       { 0.78039215686275, 0.53333333333333,  0.56470588235294 },
		rock_dark =        { 0.41960784313725, 0.38039215686275,  0.46666666666667 },
		rock_light =       { 0.49803921568627, 0.46274509803922,  0.55686274509804 },
		green_dark =       { 0.3843137254902,  0.76078431372549,  0.76078431372549 },
		green_light =      { 0.4156862745098,  0.90980392156863,  0.63529411764706 },
		jade_dark =        { 0.33725490196078, 0.52156862745098,  0.6              },
		jade_light =       { 0.29411764705882, 0.68627450980392,  0.69019607843137 },
		aqua_dark =        { 0.28627450980392, 0.46666666666667,  0.58039215686275 },
		aqua_light =       { 0.32156862745098, 0.60392156862745,  0.78039215686275 },
		pastel_dark =      { 1,                0.7921568627451,   0.83137254901961 },
		pastel_light =     { 0.80392156862745, 0.70588235294118,  0.85882352941176 },
		pink_dark =        { 0.70196078431373, 0.45098039215686,  0.64313725490196 },
		pink_light =       { 0.94901960784314, 0.57647058823529,  0.86274509803922 },
		purple_dark =      { 0.56470588235294, 0.34117647058824,  0.63921568627451 },
		purple_light =     { 0.77647058823529, 0.45098039215686,  0.8156862745098  },
		yellow_dark =      { 0.7921568627451,  0.65490196078431,  0.32156862745098 },
		yellow_light =     { 0.89803921568627, 0.75686274509804,  0.36862745098039 },
		amber_dark =       { 0.7843137254902,  0.50196078431373,  0.28627450980392 },
		amber_light =      { 0.96470588235294, 0.63921568627451,  0.18039215686275 },
		red_dark =         { 0.72549019607843, 0.35686274509804,  0.48627450980392 },
		red_light =        { 0.84313725490196, 0.33333333333333,  0.41960784313725 },
		white_dark =       { 0.84705882352941, 0.74509803921569,  0.61960784313725 },
		white_light =      { 0.96470588235294, 0.91372549019608,  0.80392156862745 },
		blue_dark =        { 0.2078431372549,  0.31372549019608,  0.43921568627451 },
		blue_light =       { 0.19607843137255, 0.61176470588235,  0.78823529411765 },
		alert_dark =       { 0.22352941176471, 0.098039215686275, 0.2              },
		alert_light =      { 0.74901960784314, 0.21960784313725,  0.49019607843137 },
	},
	catppuccin = { -- macchiato
		rosewater = { 0.95686274509804,  0.85882352941176,  0.83921568627451 },
		flamingo =  { 0.94117647058824,  0.77647058823529,  0.77647058823529 },
		pink =      { 0.96078431372549,  0.74117647058824,  0.90196078431373 },
		mauve =     { 0.77647058823529,  0.62745098039216,  0.96470588235294 },
		red =       { 0.92941176470588,  0.52941176470588,  0.58823529411765 },
		maroon =    { 0.93333333333333,  0.6,               0.62745098039216 },
		peach =     { 0.96078431372549,  0.66274509803922,  0.49803921568627 },
		yellow =    { 0.93333333333333,  0.83137254901961,  0.62352941176471 },
		green =     { 0.65098039215686,  0.85490196078431,  0.5843137254902  },
		teal =      { 0.54509803921569,  0.83529411764706,  0.7921568627451  },
		sky =       { 0.56862745098039,  0.84313725490196,  0.89019607843137 },
		sapphire =  { 0.49019607843137,  0.76862745098039,  0.89411764705882 },
		blue =      { 0.54117647058824,  0.67843137254902,  0.95686274509804 },
		lavender =  { 0.71764705882353,  0.74117647058824,  0.97254901960784 },
		text =      { 0.7921568627451,   0.82745098039216,  0.96078431372549 },
		subtext1 =  { 0.72156862745098,  0.75294117647059,  0.87843137254902 },
		subtext0 =  { 0.64705882352941,  0.67843137254902,  0.79607843137255 },
		overlay2 =  { 0.57647058823529,  0.60392156862745,  0.71764705882353 },
		overlay1 =  { 0.50196078431373,  0.52941176470588,  0.63529411764706 },
		overlay0 =  { 0.43137254901961,  0.45098039215686,  0.55294117647059 },
		surface2 =  { 0.35686274509804,  0.37647058823529,  0.47058823529412 },
		surface1 =  { 0.28627450980392,  0.30196078431373,  0.3921568627451  },
		surface0 =  { 0.21176470588235,  0.22745098039216,  0.30980392156863 },
		base =      { 0.14117647058824,  0.15294117647059,  0.22745098039216 },
		mantle =    { 0.11764705882353,  0.12549019607843,  0.18823529411765 },
		crust =     { 0.094117647058824, 0.098039215686275, 0.14901960784314 },
	}
}

---Creates pitch frequencies for UiSound
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

--#endregion
--#region Arithmetic

---Sigmoid function, Can be used for juicy UI and smooth easing among other things.
---
---https://www.desmos.com/calculator/cmmwrjtyit?invertedColors
---@param v number? Input number, if nil then it will be a Random number between 0 and 1
---@param max number The Maximum value
---@param steep number How steep the curve is
---@param offset number The horizontal offset of the middle of the curve
---@return number
function AutoSigmoid(v, max, steep, offset)
	v = AutoDefault(v, math.random(0, 10000) / 10000)
	return (max or 1) / (1 + math.exp((v - (offset or 0.5)) * (steep or -10)))
end

---Returns a smoothed value from 0 to 1,
---
---If `x` is less than `edge_1`, output `0`.
---
---If `x` is more than `edge_2`, output `1`.
---
---If `x` is between, interpolate between the two edges
---
---This is the "smootherstep" varation
---@param x number
---@param edge_1 number
---@param edge_2 number
---@return number
function AutoSmoothStep(x, edge_1, edge_2)
	x = AutoClamp((x - edge_1) / (edge_2 - edge_1))
	return x ^ 3 * (x * (6.0 * x - 15.0) + 10.0)
end

---Basically smooth step between `edge_1` and `edge_2`, then smooth step between `edge_2` and `edge_3`
---
---If `x` is less than `edge_1`, output `0`.
---
---If `x` is `edge_2`, output `1`
---
---If `x` is more than `edge_3`, output `0`.
---
---If `x` is between, interpolate between the two closest edges
---
---@param x number
---@param edge_1 number
---@param edge_2 number
---@param edge_3 number
---@return number
function AutoSmoothStep3(x, edge_1, edge_2, edge_3)
	local t
	if x < edge_2 then
		t = AutoSmoothStep(x, edge_1, edge_2)
	else
		t = 1.0 - AutoSmoothStep(x, edge_2, edge_3)
	end
	return t
end

---Rounds a number.
---
---This was a Challenge by @TallTim and @1ssnl to make the smallest rounding function, but I expanded it to make it easier to read and a little more efficent
---@param v number Input number
---@param increment number? The lowest increment. A Step of 1 will round the number to 1, A step of 5 will round it to the closest increment of 5, A step of 0.1 will round to the tenth. Default is 1
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
---@param clamp boolean? Clamp the number between b1 and b2, Default is false
---@return number
function AutoMap(v, a1, a2, b1, b2, clamp)
	clamp = AutoDefault(clamp, false)
	if a1 == a2 then return b2 end
	local mapped = b1 + ((v - a1) * (b2 - b1)) / (a2 - a1)
	return clamp and AutoClamp(mapped, math.min(b1, b2), math.max(b1, b2)) or mapped
end

---Limits a value from going below the min and above the max
---@param v number The number to clamp
---@param min number? The minimum the number can be, Default is 0
---@param max number? The maximum the number can be, Default is 1
---@return number
function AutoClamp(v, min, max)
	min = AutoDefault(min, 0)
	max = AutoDefault(max, 1)
	return math.max(math.min(v, max), min)
end

---Limits a value from going below the min and above the max
---@param v number The number to clamp
---@param max number? The maximum the length of the number can be, Default is 1
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

function AutoLengthSmallest(...)
	local args = {...}
	local minAbsValue = math.abs(args[1])

	for i = 2, #args do
		local absValue = math.abs(args[i])
		if absValue < minAbsValue then
			minAbsValue = absValue
		end
	end

	return minAbsValue
end

function AutoLengthLargest(...)
	local args = {...}
	local maxAbsValue = math.abs(args[1])

	for i = 2, #args do
		local absValue = math.abs(args[i])
		if absValue > maxAbsValue then
			maxAbsValue = absValue
		end
	end

	return maxAbsValue
end

---Wraps a value inbetween a range, Thank you iaobardar for the Optimization
---@param v number The number to wrap
---@param min number? The minimum range
---@param max number? The maximum range
---@return number
function AutoWrap(v, min, max)
	min = AutoDefault(min, 0)
	max = AutoDefault(max, 1)
	
	return (v - min) % ((max + 1) - min) + min
end

---Linerarly Iterpolates between `a` and `b` by fraction `t`
---
---Does not clamp
---@param a number Goes from number A
---@param b number To number B
---@param t number Interpolated by T
---@return number
function AutoLerp(a, b, t)
	return (1 - t) * a + t * b
end

---Spherically Iterpolates between `a` and `b` by fraction `t`.
---
---Basically Lerp but with wrapping
---@param a number Goes from number A
---@param b number To number B
---@param t number Interpolated by T
---@param w number Wraps at
---@return number
function AutoLerpWrap(a, b, t, w)
	local m = w
	local da = (b - a) % m
	local n = (da * 2) % m - da
	return a + n * t
end

---Moves `a` towards `b` by amount `t`
---
---Will clamp as to not overshoot
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

---Exponential Decay function.
---
---As seen in Freya Holmér's talk at Guadalindie
---
---https://youtu.be/LSNQuFEDOyQ
---@param a number
---@param b number
---@param decay number
---@param dt number
---@return number
function AutoExponentialDecay(a, b, decay, dt)
	return b + (a - b) * math.exp(-decay * dt)
end

---Return the Distance between the numbers `a` and `b`
---@param a number
---@param b number
---@return number
function AutoDist(a, b)
	return math.abs(a - b)
end

---Normalizes all values in a table to have a magnitude of 1 - Scales every number to still represent the same "direction"
---@param t table<number>
---@param scale number?
---@param zero_compatability boolean? If the length is zero, do not normalize values
---@return table
function AutoNormalize(t, scale, zero_compatability)
	local norm = {}
	local maxabs = 0
	for i = 1, #t do
		local abs = math.abs(t[i])
		maxabs = abs > maxabs and abs or maxabs
	end

	if zero_compatability and maxabs == 0 then maxabs = 1 end
	
	for i = 1, #t do
		norm[i] = t[i] / maxabs * (scale or 1)
	end
	return norm
end

---Takes a table of weights, like {1, 2, 0.5, 0.5}, and produces a table of how much space each weight would take up if it were to span over a given length.
---If given the weights {1, 2, 0.5, 0.5}, with a span length of 100, the resulting table would be = {25, 50, 12.5, 12.5}.
---A padding parameter can also be added which can be used to make Ui easier. Iterate through the resulting table, after each UiRect, move the width + the padding parameter
---@param weights table<number>|number weights
---@param span number
---@param padding number?
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

---Returns index of the selected weight using a bias based on the weight values. Good for Biased Randomness
---@param weights table<number>
---@return number selected
function AutoBias(weights)
	local T = {}
	local max = 0
	for i = 1, #weights do
		max = max + weights[i]
		T[i] = {}
		T[i].i = i
		T[i].w = weights[i]
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
--#endregion
--#region Vector Functions

---Rebuilds a table in a given order, also known as Swizzling
---
---| Swizzle | Result |
---| --- | --- |
---| `xyz` | { x, y, z } |
---| `zxy` | { z, x, y } |
---| `xy` | { x, y } |
---| `xz` | { x, z } |
---| `xxx` | { x, x, x } |
---| `xyzw` | { x, y, z, w } |
---| `wxyz` | { w, x, y, z } |
---| `rgba` | { r, g, b, a } |
---| `bgra` | { b, g, r, a } |
---| `aaaa` | { a, a, a, a } |
---
---@param vec vector|table
---@param swizzle string
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

---Returns true if each axis of vector `a` is equal to each axis of vector `b`
---@param a vector
---@param b vector
---@return boolean
function AutoVecEquals(a, b)
	for i, va in pairs(a) do
		if va ~= b[i] then return false end
	end
	
	return true
end

---Return a Random Vector with an optional offset and scale
---
---Not equally distributed! Uses `AutoVecRndSpherical` instead!
---
---```
---AutoVecRnd()
---AutoVecRnd(2)
---AutoVecRnd(Vec(0, 1), 0.5)
---```
---@param param1 number|vector?
---@param param2 number?
---@return vector
function AutoVecRnd(param1, param2)
	local offset, scale
	if type(param1) == "table" then
		offset = param1
		scale = param2 or 1
	else
		offset = { 0, 0, 0 }
		scale = param1 or 1
	end
	
	local rndVec = VecNormalize({
		(math.random() * 2 - 1),
		(math.random() * 2 - 1),
		(math.random() * 2 - 1),
	})
	
	return VecAdd(offset, VecScale(rndVec, scale))
end

---Return ask Random Vector on a Sphere with an optional offset and scale
---
---Ensures more even distribution
---
---```
---AutoVecRndSpherical()
---AutoVecRndSpherical(2)
---AutoVecRndSpherical(Vec(0, 1), 0.5)
---```
---@param param1 number|vector?
---@param param2 number?
---@return vector
function AutoVecRndSpherical(param1, param2)
	local offset, scale
	if type(param1) == "table" then
		offset = param1
		scale = param2 or 1
	else
		offset = { 0, 0, 0 }
		scale = param1 or 1
	end
	
	local theta = math.random() * 2 * math.pi
	local phi = math.acos(math.random() * 2 - 1)

	local v = {
		math.sin(phi) * math.cos(theta),
		math.sin(phi) * math.sin(theta),
		math.cos(phi)
	}

	return VecAdd(offset, VecScale(v, scale))
end

--- Return a random vector pointing towards the negative Z axis, randomized between two angles (in radians)
---
--- Ensures even distribution within the specified angle range
---
---@param base_vector table The base vector to randomize (optional)
---@param from_rad number The starting angle in radians
---@param to_rad number The ending angle in radians
---@param force_theta number? Forces the "z" rotation
---@return table A random vector within the specified hemisphere
function AutoVecRndAngle(base_vector, from_rad, to_rad, force_theta)
	local rad = math.pi - AutoMap(math.random(), 0, 1, from_rad, to_rad) / 2
	local theta = force_theta or (math.random() * 2 * math.pi)

	local v = {
		math.sin(rad) * math.cos(theta),
		math.sin(rad) * math.sin(theta),
		math.cos(rad)
	}

	if base_vector then
		local quat = QuatLookAt(nil, VecNormalize(base_vector))
		return VecScale(QuatRotateVec(quat, v), VecLength(base_vector))
	else
		v[3] = -v[3]
		return v
	end
end


---Return the Distance between Two Vectors
---@param a vector
---@param b vector
---@return number
function AutoVecDist(a, b)
	return VecLength(VecSub(b, a))
end

---Return the Distance between Two Vectors, without considering the X component
---@param a vector
---@param b vector
---@return number
function AutoVecDistNoX(a, b)
	return math.sqrt((b[2] - a[2])^2 + (b[3] - a[3])^2)
end

---Return the Distance between Two Vectors, without considering the Y component
---@param a vector
---@param b vector
---@return number
function AutoVecDistNoY(a, b)
	return math.sqrt((b[1] - a[1])^2 + (b[3] - a[3])^2)
end

---Return the Distance between Two Vectors, without considering the Z component
---@param a vector
---@param b vector
---@return number
function AutoVecDistNoZ(a, b)
	return math.sqrt((b[1] - a[1])^2 + (b[2] - a[2])^2)
end

---Moves a vector in a direction by a given amount
---
---Equivalent to `VecAdd(vec, VecScale(dir, dist))`
---@param vec any
---@param dir any
---@param dist any
---@return vector
function AutoVecMove(vec, dir, dist)
	return VecAdd(vec, VecScale(dir, dist))
end

---Returns a Vector Rounded to a number
---@param vec vector
---@param r number?
---@return vector
function AutoVecRound(vec, r)
	return Vec(AutoRound(vec[1], r), AutoRound(vec[2], r), AutoRound(vec[3], r))
end

---Returns a Vector where all numbers are floored
---@param vec vector
---@return vector
function AutoVecFloor(vec)
	return Vec(math.floor(vec[1]), math.floor(vec[2]), math.floor(vec[3]))
end

---Returns a Vector where all numbers are ceiled
---@param vec vector
---@return vector
function AutoVecCeil(vec)
	return Vec(math.ceil(vec[1]), math.ceil(vec[2]), math.ceil(vec[3]))
end

---Return a vector that has the magnitude of `b`, but with the direction of `a`
---
---Equivalent to `VecScale(VecNormalize(a), b)`
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
---@param min number? The minimum the magnitude can be, Default is 0
---@param max number? The maximum the magnitude can be, Default is 1
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
---@param min number? The minimum, Default is 0
---@param max number? The maximum, Default is 1
---@return vector
function AutoVecClamp(v, min, max)
	min, max = AutoDefault(min, 0), AutoDefault(max, 1)
	return {
		AutoClamp(v[1], min, max),
		AutoClamp(v[2], min, max),
		AutoClamp(v[3], min, max)
	}
end

---Limits a vector to be between min and max vector
---@param v vector The Vector to clamp
---@param min vector The minimum
---@param max vector The maximum
---@return vector
function AutoVecClampVec(v, min, max)
	return {
		AutoClamp(v[1], min[1], max[1]),
		AutoClamp(v[2], min[2], max[2]),
		AutoClamp(v[3], min[3], max[3])
	}
end

---Return Vec(1, 1, 1) scaled by length
---@param length number return the vector of size length, Default is 1
---@return vector
function AutoVecOne(length)
	local l = length or 1
	return Vec(l, l, l)
end

---Returns the midpoint between two vectors
---
---Equivalent to `VecScale(VecAdd(a, b), 0.5)`
---@param a any
---@param b any
---@return vector
function AutoVecMidpoint(a, b)
	return VecScale(VecAdd(a, b), 0.5)
end

---Return Vec `a` multiplied by Vec `b`
---@param a vector
---@param b vector
---@return vector
function AutoVecMulti(a, b)
	return {
		(a[1] or 0) * (b[1] or 0),
		(a[2] or 0) * (b[2] or 0),
		(a[3] or 0) * (b[3] or 0),
	}
end

---Return Vec `a` divided by Vec `b`
---@param a vector
---@param b vector
---@return vector
function AutoVecDiv(a, b)
	return {
		(a[1] or 0) / (b[1] or 0),
		(a[2] or 0) / (b[2] or 0),
		(a[3] or 0) / (b[3] or 0),
	}
end

---Return Vec `a` to the Power of `b`
---@param a vector
---@param b number
---@return vector
function AutoVecPow(a, b)
	return {
		(a[1] or 0) ^ (b or 0),
		(a[2] or 0) ^ (b or 0),
		(a[3] or 0) ^ (b or 0),
	}
end

---Return Vec `a` to the Power of Vec `b`
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

---Returns the absolute value of an vector
---@param v vector
---@return vector
function AutoVecAbs(v)
	return {
		math.abs(v[1]),
		math.abs(v[2]),
		math.abs(v[3]),
	}
end

---Equivalent to `math.min(unpack(v))`
---@param v vector
---@return number
function AutoVecMin(v)
	return math.min(unpack(v))
end

---Equivalent to `math.max(unpack(v))`
---@param v vector
---@return number
function AutoVecMax(v)
	return math.max(unpack(v))
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

-- Function to perform spherical interpolation between two normalized vectors
function AutoVecSlerp(v1, v2, t)
	-- Ensure vectors are normalized
	v1 = VecNormalize(v1)
	v2 = VecNormalize(v2)

	-- Calculate the dot product
	local dotProduct = VecDot(v1, v2)

	-- Clamp the dot product to ensure it stays within [-1, 1]
	dotProduct = math.max(math.min(dotProduct, 1), -1)

	-- Calculate the angle between the two vectors
	local theta = math.acos(dotProduct)

	-- If the angle is zero, return either vector
	if theta == 0 then
		return v1
	end

	-- Interpolate
	local sinTheta = math.sin(theta)
	local coef1 = math.sin((1 - t) * theta) / sinTheta
	local coef2 = math.sin(t * theta) / sinTheta

	local result = {
		v1[1] * coef1 + v2[1] * coef2,
		v1[2] * coef1 + v2[2] * coef2,
		v1[3] * coef1 + v2[3] * coef2
	}

	-- Normalize the result and return
	return VecNormalize(result)
end

---Return `v` with it's `x` value replaced by `subx`
---@param v vector
---@param subx number
function AutoVecSubsituteX(v, subx)
	local new = VecCopy(v)
	new[1] = subx
	return new
end

---Return `v` with it's `y` value replaced by `suby`
---@param v vector
---@param suby number
function AutoVecSubsituteY(v, suby)
	local new = VecCopy(v)
	new[2] = suby
	return new
end

---Return `v` with it's `z` value replaced by `subz`
---@param v vector
---@param subz number
function AutoVecSubsituteZ(v, subz)
	local new = VecCopy(v)
	new[3] = subz
	return new
end

--- Projects a vector onto a plane defined by its normal.
---@param vector vector The vector to be projected.
---@param planeNormal vector The normal vector of the plane.
---@param rescale boolean? Rescale the projected vector to be the same length as the provided vector
---@return vector projectedVector The projected vector.
function AutoVecProjectOntoPlane(vector, planeNormal, rescale)
	local dotProduct = VecDot(vector, planeNormal)
	local projection = VecScale(planeNormal, dotProduct)
	local projectedVector = VecSub(vector, projection)
	if rescale then return AutoVecRescale(projectedVector, VecLength(vector)) else return projectedVector end
end



---Converts the output of VecDot with normalized vectors to an angle
---@param dot number
---@return number
function AutoDotToAngle(dot)
	return math.deg(math.acos(dot))
end

--#endregion
--#region Quat Functions

---Equivalent to `QuatRotateVec(rot, Vec(0, 0, 1))`
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

---Computes the dot product of two quaternions.
---@param a quaternion
---@param b quaternion
---@return number
function AutoQuatDot(a, b)
	return a[1] * b[1] + a[2] * b[2] + a[3] * b[3] + a[4] * b[4]
end

---Returns the Conjugate of the given quaternion.
---@param quat quaternion
---@return quaternion quat
function AutoQuatConjugate(quat)
	return { -quat[1], -quat[2], -quat[3], quat[4] }
end

---Returns the Inverse of the given quaternion.
---@param quat quaternion
---@return quaternion quat
function AutoQuatInverse(quat)
	local norm = quat[1] ^ 2 + quat[2] ^ 2 + quat[3] ^ 2 + quat[4] ^ 2
	local inverse = { -quat[1] / norm, -quat[2] / norm, -quat[3] / norm, quat[4] / norm }
	return inverse
end

---@param quat_to_inverse quaternion
---@param quat quaternion
---@return quaternion
function AutoQuatInverseRotate(quat_to_inverse, quat)
	return QuatRotateQuat(AutoQuatInverse(quat_to_inverse), quat)
end

---@param quat quaternion
---@param scalar number
function AutoQuatScale(quat, scalar)
	return QuatSlerp(Quat(), quat, scalar)
end

---Between -a and a, picks the quaternion nearest to b
---@param a quaternion
---@param b quaternion
---@return quaternion
---
---Thankyou to Mathias for this function
function AutoQuatNearest(a, b)
	return AutoQuatDot(a, b) < 0 and { -a[1], -a[2], -a[3], -a[4] } or { a[1], a[2], a[3], a[4] }
end

--- Returns the angle of which a quaternion is rotated about a given axis
---
--- Credit goes to Mathias, thank you!
function AutoQuatAngleAboutAxis(q, axis)
	local qXYZ = Vec(q[1], q[2], q[3])
	local co = q[4]
	local si = VecDot(axis, qXYZ) / VecDot(axis, axis)

	if si == 0 then return 0 end

	-- eliminate double-cover
	if co < 0 then
		co = -co
		si = -si
	end

	return math.deg(2.0 * math.atan2(si, co))
end

---Same as `QuatAxisAngle()` but takes a single vector instead of a unit vector + an angle, for convenience
---
---Thankyou to Mathias for this function
---@param v any
---@return quaternion
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

---Converts a quaternion to an axis angle representation
---Returns a rotation vector where axis is the direction and angle is the length
---
---Thankyou to Mathias for this function
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

--#endregion
--#region AABB Bounds Functions

---Get the center of a body's bounds
---@param body body_handle
---@return vector
function AutoBodyCenter(body)
	local aa, bb = GetBodyBounds(body)
	return VecScale(VecAdd(aa, bb), 0.5)
end

---Get the center of a shapes's bounds
---@param shape shape_handle
---@return vector
function AutoShapeCenter(shape)
	local aa, bb = GetShapeBounds(shape)
	return VecScale(VecAdd(aa, bb), 0.5)
end

---Expands a given boudns to include a point
---@param aa vector
---@param bb vector
---@param ... vector Points, can be one or multiple
---@return vector
---@return vector
function AutoAABBInclude(aa, bb, ...)
	for _, point in ipairs(arg) do
		aa, bb = {
			math.min(aa[1], point[1]),
			math.min(aa[2], point[2]),
			math.min(aa[3], point[3]),
		}, {
			math.max(bb[1], point[1]),
			math.max(bb[2], point[2]),
			math.max(bb[3], point[3]),
		}
	end
	
	return aa, bb
end

---Returns a Axis ALigned Bounding Box with the center of pos
---@param pos vector
---@param halfextents vector|number
---@return vector lower-bound
---@return vector upper-bound
function AutoAABBBoxFromPoint(pos, halfextents)
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
---@param vec vector? A normalized Vector pointing towards the position that should be retrieved, Default is Vec(0, 0, 0)
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
function AutoAABBSubdivideBounds(aa, bb, levels)
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

---@param point vector
---@param lower_bound vector
---@param upper_bound vector
---@return boolean
function AutoAABBInside(lower_bound, upper_bound, point)
	for i = 1, 3 do
		if point[i] < lower_bound[i] or point[i] > upper_bound[i] then
			return false
		end
	end
	
	return true
end


---@param lower_bound vector
---@param upper_bound vector
---@param start_pos vector
---@param direction vector
---@param trigger_inside boolean
---@return { hit:boolean, intersection:vector, normal:vector, dist:number }
function AutoRaycastAABB(lower_bound, upper_bound, start_pos, direction, trigger_inside)
	local t1 = (lower_bound[1] - start_pos[1]) / direction[1]
	local t2 = (upper_bound[1] - start_pos[1]) / direction[1]

	local tmin = math.min(t1, t2)
	local tmax = math.max(t1, t2)

	for i = 2, 3 do
		t1 = (lower_bound[i] - start_pos[i]) / direction[i]
		t2 = (upper_bound[i] - start_pos[i]) / direction[i]

		tmin = math.max(tmin, math.min(t1, t2))
		tmax = math.min(tmax, math.max(t1, t2))
	end

	local inside = tmin < 0 and tmax >= 0

	if inside or (tmax >= tmin and tmax >= 0) then
		local intersection
		local normal

		if inside and not trigger_inside then
			return { hit = false }
		else
			intersection = VecAdd(start_pos, VecScale(direction, tmin))
			normal = { 0, 0, 0 }

			for i = 1, 3 do
				if intersection[i] == lower_bound[i] then
					normal[i] = -1
				elseif intersection[i] == upper_bound[i] then
					normal[i] = 1
				end
			end
		end

		return {
			hit = true,
			intersection = intersection,
			normal = normal,
			dist = tmin
		}
	end

	return { hit = false }
end


---Draws a given Axis Aligned Bounding Box
---@param aa vector lower-bound
---@param bb vector upper-bound
---@param colorR number?
---@param colorG number?
---@param colorB number?
---@param alpha number?
---@param rgbcolors boolean?
---@param draw boolean?
function AutoDrawAABB(aa, bb, colorR, colorG, colorB, alpha, rgbcolors, draw)
	colorR = AutoDefault(colorR, 0)
	colorG = AutoDefault(colorG, 0)
	colorB = AutoDefault(colorB, 0)
	alpha = AutoDefault(alpha, 1)
	rgbcolors = AutoDefault(rgbcolors, false)
	draw = AutoDefault(draw, false)
	
	local min, max = {
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

--#endregion
--#region OBB Bounds Functions

---@class OBB: { pos:vector, rot:quaternion, size:vector }|transform

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

---Returns the planes and corners representing the faces of a Oriented Bounding Box
---@param obb OBB
---@return { z:plane, zn:plane, x:plane, xn:plane, y:plane, yn:plane }
---@return { xyz:table, Xyz:table, xYz:table, xyZ:table, XYz:table, XyZ:table, xYZ:table, XYZ:table }
function AutoGetOBBFaces(obb)
	local corners = AutoGetOBBCorners(obb)
	
	local faces = {}
	faces.z = AutoPlane(
		VecLerp(corners.xyZ, corners.XYZ, 0.5),
		QuatRotateQuat(obb.rot, QuatEuler(180, 0, 0)),
		{ obb.size[1], obb.size[2] }
	)
	faces.zn = AutoPlane(
		VecLerp(corners.xyz, corners.XYz, 0.5),
		QuatRotateQuat(obb.rot, QuatEuler(0, 0, 0)),
		{ obb.size[1], obb.size[2] }
	)
	faces.x = AutoPlane(
		VecLerp(corners.Xyz, corners.XYZ, 0.5),
		QuatRotateQuat(obb.rot, QuatEuler(0, -90, -90)),
		{ obb.size[2], obb.size[3] }
	)
	faces.xn = AutoPlane(
		VecLerp(corners.xyz, corners.xYZ, 0.5),
		QuatRotateQuat(obb.rot, QuatEuler(0, 90, 90)),
		{ obb.size[2], obb.size[3] }
	)
	faces.y = AutoPlane(
		VecLerp(corners.xYz, corners.XYZ, 0.5),
		QuatRotateQuat(obb.rot, QuatEuler(90, 0, 0)),
		{ obb.size[1], obb.size[3] }
	)
	faces.yn = AutoPlane(
		VecLerp(corners.xyz, corners.XyZ, 0.5),
		QuatRotateQuat(obb.rot, QuatEuler(-90, 180, 0)),
		{ obb.size[1], obb.size[3] }
	)

	return faces, corners
end

---Returns a table representing the lines connecting the sides of a Oriented Bounding Box
---@param obb OBB
---@return table<{ [1]:vector, [2]:vector }>
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

---@param shape shape_handle
---@return OBB
function AutoGetShapeOBB(shape, local_space)
	local transform = local_space and Transform() or GetShapeWorldTransform(shape)
	local x, y, z, scale = GetShapeSize(shape)
	local size = VecScale(Vec(x, y, z), scale)
	
	local center = TransformToParentPoint(transform, VecScale(size, 0.5))
	return AutoOBB(center, transform.rot, size)
end

---Draws a given Oriented Bounding Box
---@param obb OBB
---@param red number? Default is 0
---@param green number? Default is 0
---@param blue number? Default is 0
---@param alpha number? Default is 1
---@param linefunction function? Default is DebugLine
function AutoDrawOBB(obb, red, green, blue, alpha, linefunction)
	local lines = AutoOBBLines(obb)
	
	linefunction = linefunction or DebugLine
	for k, l in pairs(lines) do
		linefunction(l[1], l[2], red or 0, green or 0, blue or 0, alpha or 1)
	end
end

--#endregion
--#region Plane Functions

---@class plane: { pos:vector, rot:quaternion, size:{ [1]:number, [2]:number } }|transform

---@param pos vector
---@param rot quaternion
---@param size { [1]:number, [2]:number }
---@return plane
function AutoPlane(pos, rot, size)
	return { pos = pos or Vec(), rot = rot or Quat(), size = size or { 1, 1 } }
end

---@param plane plane
---@return { [1]:vector, [2]:vector, [3]:vector, [4]:vector }
function AutoGetPlaneCorners(plane)
	local size = VecScale(plane.size, 0.5)
	
	local corner1 = Vec(-size[1], -size[2])
	local corner2 = Vec(size[1], -size[2])
	local corner3 = Vec(size[1], size[2])
	local corner4 = Vec(-size[1], size[2])
	
	-- Rotate corners using the quaternion rotation from the plane object
	corner1 = TransformToParentPoint(plane, corner1)
	corner2 = TransformToParentPoint(plane, corner2)
	corner3 = TransformToParentPoint(plane, corner3)
	corner4 = TransformToParentPoint(plane, corner4)
	
	return { corner1, corner2, corner3, corner4 }
end

---@param plane plane
---@param startPos vector
---@param direction vector
---@param oneway boolean?
---@return { hit:boolean, intersection:vector, normal:vector, dist:number, dot:number }
function AutoRaycastPlane(plane, startPos, direction, oneway)
	local pos = plane.pos or Vec(0, 0, 0)
	local rot = plane.rot or Quat()
	local size = plane.size or Vec(1, 1, 1)
	
	local halfsize = VecScale(size, 0.5)
	local corner1 = VecAdd(pos, QuatRotateVec(rot, Vec(-halfsize[1], -halfsize[2], 0)))
	local corner2 = VecAdd(pos, QuatRotateVec(rot, Vec(halfsize[1], -halfsize[2], 0)))
	local corner3 = VecAdd(pos, QuatRotateVec(rot, Vec(halfsize[1], halfsize[2], 0)))
	local corner4 = VecAdd(pos, QuatRotateVec(rot, Vec(-halfsize[1], halfsize[2], 0)))
	
	local normal = QuatRotateVec(rot, { 0, 0, -1 })
	
	local rayDirDotNormal = VecDot(direction, normal)
	if (oneway and rayDirDotNormal or math.abs(rayDirDotNormal)) < 0 then
		-- Ray is parallel to plane, or wrong way; no intersection
		return { hit = false, normal = normal, dist = 1 / 0, dot = rayDirDotNormal }
	else
		local rayToPlane = VecSub(startPos, pos)
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

---@param plane plane
---@param pattern 0|1|2|3
---@param patternstrength number
---@param oneway boolean?
---@param draw_function function?
---@param r number?
---@param g number?
---@param b number?
---@param a number?
function AutoDrawPlane(plane, pattern, patternstrength, oneway, draw_function, r, g, b, a)
	local pos = plane.pos or Vec(0, 0, 0)
	local rot = plane.rot or Quat()
	local size = plane.size or Vec(1, 1, 1)
	
	-- Calculate the forward, right, and up vectors from the rotation
	local forward = QuatRotateVec(rot, Vec(0, 0, 1))
	
	-- Cancel for oneway
	if oneway and VecDot(VecSub(pos, GetCameraTransform().pos), forward) < 0 then
		return
	end
	
	local right = QuatRotateVec(rot, Vec(1, 0, 0))
	local up = QuatRotateVec(rot, Vec(0, 1, 0))
	
	-- Calculate the corners of the plane
	local corner1 = VecAdd(VecAdd(pos, VecScale(right, -size[1] / 2)), VecScale(up, -size[2] / 2))
	local corner2 = VecAdd(VecAdd(pos, VecScale(right, size[1] / 2)), VecScale(up, -size[2] / 2))
	local corner3 = VecAdd(VecAdd(pos, VecScale(right, size[1] / 2)), VecScale(up, size[2] / 2))
	local corner4 = VecAdd(VecAdd(pos, VecScale(right, -size[1] / 2)), VecScale(up, size[2] / 2))
	
	r, g, b, a = r or 1, g or 1, b or 1, a or 1
	pattern = pattern or 0
	
	draw_function = draw_function or DebugLine
	
	-- Draw the grid
	if pattern == 0 then
		patternstrength = (patternstrength or 0) + 1
		for i = 0, patternstrength do
			local subH1 = VecLerp(corner1, corner2, i / patternstrength)
			local subH2 = VecLerp(corner4, corner3, i / patternstrength)
			local subV1 = VecLerp(corner1, corner4, i / patternstrength)
			local subV2 = VecLerp(corner2, corner3, i / patternstrength)
			
			draw_function(subH1, subH2, r, g, b, a)
			draw_function(subV1, subV2, r, g, b, a)
		end
	elseif pattern > 0 then
		draw_function(corner1, corner2, r, g, b, a)
		draw_function(corner2, corner3, r, g, b, a)
		draw_function(corner3, corner4, r, g, b, a)
		draw_function(corner4, corner1, r, g, b, a)
	end
	
	if pattern == 1 or pattern == 3 then
		patternstrength = (patternstrength or 1)
		local step = 1 / patternstrength
		for t = step, 2, step * 2 do
			local p1 = t <= 1 and VecLerp(corner1, corner2, t) or VecLerp(corner2, corner3, t - 1)
			local p2 = t <= 1 and VecLerp(corner1, corner4, t) or VecLerp(corner4, corner3, t - 1)
			
			draw_function(p1, p2, r, g, b, a)
		end
	end
	
	if pattern == 2 or pattern == 3 then
		patternstrength = (patternstrength or 0)
		local step = 1 / patternstrength
		for t = step, 2, step * 2 do
			local p1 = t <= 1 and VecLerp(corner2, corner3, t) or VecLerp(corner3, corner4, t - 1)
			local p2 = t <= 1 and VecLerp(corner2, corner1, t) or VecLerp(corner1, corner4, t - 1)
			
			draw_function(p1, p2, r, g, b, a)
		end
		
		draw_function(corner1, corner2, r, g, b, a)
		draw_function(corner2, corner3, r, g, b, a)
		draw_function(corner3, corner4, r, g, b, a)
		draw_function(corner4, corner1, r, g, b, a)
	end
end

--#endregion
--#region Sphere Functions

function AutoFibonacciSphere(numPoints, sphere_origin, sphere_radius, y_multi)
	local points = {}
	local phi = math.pi * (3.0 - math.sqrt(5.0)) -- Golden angle in radians

	for i = 0, numPoints - 1 do
		local y = 1 - (i / (numPoints - 1)) * 2 * (y_multi or 1) -- Range from -1 to 1
		local radiusY = math.sqrt(1 - y * y)
		local theta = phi * i -- Golden angle increment

		local x = math.cos(theta) * radiusY
		local z = math.sin(theta) * radiusY

		points[i] = {
			x * sphere_radius + sphere_origin[1],
			y * sphere_radius + sphere_origin[2],
			z * sphere_radius + sphere_origin[3],
		}
	end

	return points
end

---@param sphere_origin vector Origin of the sphere
---@param sphere_radius number Radius of the sphere
---@param quality number? Default is 1
---@param draw_function function? Function used for drawing. Default is `DebugLine`
---@vararg any draw_parameters Parameters fed into `draw_function`
function AutoDrawSphereTangent(sphere_origin, sphere_radius, quality, force_view_transform, draw_function, ... )
	local view = force_view_transform or GetCameraTransform()
	local camera_up = AutoTransformUp(view)
	local diff_to_camera = VecSub(view.pos, sphere_origin)
	local dist = VecLength(diff_to_camera)
	local dir = VecNormalize(diff_to_camera)
	local cross = VecCross(camera_up, dir)
	draw_function = draw_function or DebugLine
	
	local step_size = ((math.max(dist, 1) ^ 0.5) / (64 * (quality or 1)) / (math.max(sphere_radius ^ 0.5, 0.01)))* 360
	local last_point
	for a = 0, 360, step_size do
		local next_point = VecAdd(sphere_origin, QuatRotateVec(QuatAxisAngle(dir, a), VecScale(cross, sphere_radius)))
		if last_point then draw_function(last_point, next_point, ... ) end
		last_point = next_point
	end

	draw_function(last_point, VecAdd(sphere_origin, VecScale(cross, sphere_radius)), ... )
end

---@param sphere_origin vector
---@param sphere_radius number
---@param startPos vector
---@param direction vector
---@return { hit:boolean, intersections:{ [1]:vector, [2]:vector }, normals:{ [1]:vector, [2]:vector }, dists:{ [1]:number, [2]:number } }
function AutoRaycastSphere(sphere_origin, sphere_radius, startPos, direction)
	local center = sphere_origin or Vec(0, 0, 0)
	local radius = sphere_radius or 1

	local rayToSphere = VecSub(center, startPos)
	local tca = VecDot(rayToSphere, direction)
	local d2 = VecDot(rayToSphere, rayToSphere) - tca * tca

	if d2 > radius * radius then
		-- No intersection with sphere
		return { hit = false }
	end

	local thc = math.sqrt(radius * radius - d2)
	local t0 = tca - thc
	local t1 = tca + thc

	local intersections = {}
	local normals = {}
	local dists = {}

	if t0 >= 0 then
		local intersection = VecAdd(startPos, VecScale(direction, t0))
		local normal = VecNormalize(VecSub(intersection, center))
		local dist = AutoVecDist(startPos, intersection)

		table.insert(intersections, intersection)
		table.insert(normals, normal)
		table.insert(dists, dist)
	end

	if t1 >= 0 then
		local intersection = VecAdd(startPos, VecScale(direction, t1))
		local normal = VecNormalize(VecSub(intersection, center))
		local dist = AutoVecDist(startPos, intersection)

		table.insert(intersections, intersection)
		table.insert(normals, normal)
		table.insert(dists, dist)
	end

	return {
		hit = #intersections > 0,
		intersections = intersections,
		normals = normals,
		dists = dists,
	}
end

--#endregion
--#region Octree Functions

---Undocumented
---@param BoundsAA vector
---@param BoundsBB vector
---@param Layers number
---@param conditionalFuction function
---@param _layer number?
---@return table
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
		for _, nb in ipairs(AutoAABBSubdivideBounds(BoundsAA, BoundsBB)) do
			local aa, bb = unpack(nb)
			node.children[#node.children + 1] = AutoProcessOctree(aa, bb, Layers, conditionalFuction, _layer + 1)
		end
	end
	
	return node
end

---Undocumented
---@param aa vector
---@param bb vector
---@return boolean
---@return table
function AutoQueryBoundsForBody(aa, bb)
	QueryRequire('physical large')
	local mid = VecLerp(aa, bb, 0.5)
	local radius = AutoVecDist(aa, bb) * 0.707 / 2
	local hit, point, normal, shape = QueryClosestPoint(mid, radius)
	return hit, { pos = point, normal = normal, shape }
end

---Draws the Octree from AutoProcessOctree
---@param node table
---@param layer number
---@param drawfunction function?
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

--#endregion
--#region Point Physics

---Creates a Point Physics Simulation Instance
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
	---@param Position vector? Default is Vec(0, 0, 0)
	---@param Velocity vector? Default is Vec(0, 0, 0)
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
	---@param Image string|false? A image that is drawn in the position of the points, Default is 'ui/common/dot.png', if set to false, then draws a Transform at the position instead
	---@param SizeMultiplier number? a multipler for the size of the drawn image, Default is 3.5
	---@param Occlude boolean? Whether to hide points that are obscured by walls, Default is true
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

--#endregion
--#region Secondary Motion

--Previously known as Second Order System
--Huge Thanks to Mathias#1325 for work on the Quaternion Functions

---@class Secondary_Motion_Data: table

---Returns a table representing a Second Order System (SOS) that can be used to make secondary motion
---@param initial number|table<number>
---@param frequency number
---@param dampening number
---@param response number
---@param raw_k boolean?
---@return Secondary_Motion_Data
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

---Returns a table representing a Second Order System (SOS) that can be used to make secondary motion
---@param initial number|table<number>
---@param frequency number
---@param dampening number
---@param response number
---@param raw_k boolean?
---@return Secondary_Motion_Data
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
---@param sm Secondary_Motion_Data
---@param target number|table<number>
---@param timestep number?
function AutoSM_Update(sm, target, timestep)
	timestep = timestep or GetTimeStep()
	
	if sm.type ~= 'quaternion' then
		local function update(v, t)
			local xd = (t - v.previous) / timestep
			v.previous = t
			
			local k2_stable = math.max(sm.k_values[2], timestep ^ 2 / 2 + timestep * sm.k_values[1] / 2,
			timestep * sm.k_values[1])
			v.current = v.current + timestep * v.velocity
			v.velocity = v.velocity +
			timestep * (t + sm.k_values[3] * xd - v.current - sm.k_values[1] * v.velocity) / k2_stable
		end
		
		if sm.type == 'single' then
			update(sm.data, target)
		else
			for k, v in pairs(sm.data) do
				update(v, target[k])
			end
		end
	else
		-- Compute the quaternion that will rotate the last quaternion to the desired quaternion
		-- Convert it to an axis-angle rotation vector
		local q = QuatRotateQuat(AutoQuatConjugate(sm.data.previous), AutoQuatNearest(target, sm.data.previous))
		local dx = AutoQuatToAxisAngle(q)
		dx = VecScale(dx, 1 / timestep)
		
		sm.data.previous = QuatCopy(target)
		
		-- Convert our angular velocity to a quaternion
		local qVel = AutoQuatFromAxisAngle(VecScale(sm.data.velocity, timestep))
		sm.data.current = QuatRotateQuat(sm.data.current, qVel) -- Rotate
		
		-- desired - sos.data.current, in quaternion form
		local q2 = QuatRotateQuat(AutoQuatConjugate(sm.data.current), AutoQuatNearest(target, sm.data.current))
		local s = AutoQuatToAxisAngle(q2)
		local k2_stable = math.max(sm.k_values[2], timestep * timestep / 2 + timestep * sm.k_values[1] / 2, timestep * sm.k_values[1])
		
		--- "wtf" - Autumn
		sm.data.velocity = VecAdd(sm.data.velocity, VecScale(VecScale(VecAdd(s, VecSub(VecScale(dx, sm.k_values[3]), VecScale(sm.data.velocity, sm.k_values[1]))), timestep), 1 / k2_stable))
	end
end

---Returns the current value of a Second Order System
---@param sm Secondary_Motion_Data
---@return unknown
function AutoSM_Get(sm)
	if sm.type ~= 'table' then
		return sm.data.current
	else
		local values = {}
		for k, v in pairs(sm.data) do
			values[k] = v.current
		end
		
		return values
	end
end

---Returns the current velocity of a Second Order System
---@param sm Secondary_Motion_Data
---@return unknown
function AutoSM_GetVelocity(sm)
	if sm.type ~= 'table' then
		return sm.data.velocity
	else
		return AutoTableSubi(sm.data, 'velocity')
	end
end

---Sets the current values of a Second Order System
---@param sm Secondary_Motion_Data
---@param target number|table<number>|quaternion
---@param keep_velocity boolean?
function AutoSM_Set(sm, target, keep_velocity, keep_previous)
	if sm.type ~= 'table' then
		sm.data.current = target
		
		if not keep_velocity then sm.data.velocity = 0 end
		if not keep_previous then sm.data.previous = target end
	else
		for k, v in pairs(sm.data) do
			v.current = target[k]
			
			if not keep_velocity then v.velocity = 0 end
			if not keep_previous then v.previous = target[k] end
		end
	end
end

---Sets the current velocity of a Second Order System
---@param sm Secondary_Motion_Data
---@param velocity number|table<number>
function AutoSM_SetVelocity(sm, velocity)
	if sm.type == 'single' then
		sm.data.velocity = velocity
	elseif sm.type == 'quaternion' then
		sm.data.velocity = AutoEulerTable(velocity)
	else
		for k, v in pairs(sm.data) do
			v.velocity = velocity[k]
		end
	end
end

---Adds a amount to the current velocity of a Second Order System
---@param sm Secondary_Motion_Data
---@param velocity number|table<number>
function AutoSM_AddVelocity(sm, velocity)
	if sm.type == 'single' then
		sm.data.velocity = sm.data.velocity + velocity
	elseif sm.type == 'quaternion' then
		sm.data.velocity = VecAdd(sm.data.velocity, AutoEulerTable(velocity))
	else
		for k, v in pairs(sm.data) do
			v.velocity = v.velocity + velocity[k]
		end
	end
end

---Recalculates The K values for a Second Order System
---@param sm Secondary_Motion_Data
---@param frequency number
---@param dampening number
---@param response number
---@param raw_k boolean?
function AutoSM_RecalculateK(sm, frequency, dampening, response, raw_k)
	sm.k_values = {
		raw_k and frequency or (dampening / (math.pi * frequency)),
		raw_k and dampening or (1 / (2 * math.pi * frequency) ^ 2),
		raw_k and response or (response * dampening / (2 * math.pi * frequency)),
	}
end

--#endregion
--#region Table Functions

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

function AutoSelector(t, f, number_indexed_return)
	local returns = {}
	for k, v in pairs(t) do
		returns[number_indexed_return and #returns+1 or k] = f(v)
	end
	return returns
end

---Repeats a value `v`, `r` amount of times
---@param v any
---@param r integer
---@return table
function AutoTableRepeatValue(v, r)
	local t = {}
	for i=1,r do
		t[#t+1] = type(v) == 'table' and AutoTableDeepCopy(v) or v
	end
	return t
end

---Concats Table 2 onto the end of Table 1, does not return anything
---@param t1 table
---@param t2 table
function AutoTableConcat(t1, t2)
	for i = 1, #t2 do
		t1[#t1 + 1] = t2[i]
	end
end

---Merges two tables together, does not return anything
---@param base table
---@param overwrite table
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
end

---A lambda like function for returning a table's key's values.
---@param t table
---@param key any
---@return table
function AutoTableSub(t, key)
	local _t = {}
	for i, v in pairs(t) do
		_t[i] = v[key]
	end
	return _t
end

---A lambda like function for returning a table's key's values.
---Same as AutoTableSub, but uses ipairs instead
---@param t table
---@param key any
---@return table
function AutoTableSubi(t, key)
	local _t = {}
	for i, v in ipairs(t) do
		_t[i] = v[key]
	end
	return _t
end

---Swaps the keys and the values of a table
---@param t table
---@return table
function AutoTableSwapKeysAndValues(t)
	local _t = {}
	for k, v in pairs(t) do
		_t[v] = k
	end
	return _t
end

---Equivalent to
---```
---for i, v in pairs(t) do
---    v[key] = tset[i]
---end
---```
---@param t table
---@param key any
---@param tset table
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
---@param t table
---@return any
function AutoTableLast(t)
	return t[AutoTableCount(t)]
end

---Copy a Table Recursivly Stolen from http://lua-users.org/wiki/CopyTable
---@generic T : table
---@param orig T
---@param copies table?
---@return T
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

--#endregion
--#region Utility Functions

---If val is nil, return default instead
---@param v any
---@param default any
---@return any
function AutoDefault(v, default)
	if v == nil then return default else return v end
end

---Calls function or table of functions `f` and gives `...` as input parameters
---@param f function|table<function>
---@vararg any
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

---Scales a transform, is the equivelent of (s)lerping the position and rotation from Vec(), Quat()
---@param t transform
---@param s number
---@param s2 number?
---@return transform
function AutoTransformScale(t, s, s2)
	return AutoTransformLerp(Transform(Vec(), Quat()), t, s, s2)
end

---Returns a Linear Interpolated Transform, Interpolated by t.
---@param a transform
---@param b transform
---@param t number
---@param t2 number?
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

---Equivalent to `QuatRotateVec(t.rot, Vec(0, 0, -(scale or 1)))`
---@param t transform
---@param scale number?
---@return vector
function AutoTransformFwd(t, scale)
	return QuatRotateVec(t.rot, Vec(0, 0, -(scale or 1)))
end

---Equivalent to `QuatRotateVec(t.rot, Vec(0, scale or 1))`
---@param t transform
---@param scale number?
---@return vector
function AutoTransformUp(t, scale)
	return QuatRotateVec(t.rot, Vec(0, scale or 1))
end

---Equivalent to `QuatRotateVec(t.rot, Vec(scale or 1))`
---@param t transform
---@param scale number?
---@return vector
function AutoTransformRight(t, scale)
	return QuatRotateVec(t.rot, Vec(scale or 1))
end

---Equivalent to `Transform(TransformToParentPoint(t, offset), t.rot)`
---@param t transform
---@param offset vector
---@return transform
function AutoTransformOffset(t, offset)
	return Transform(TransformToParentPoint(t, offset), t.rot)
end

--- Rotates a transform around a given point
---@param transform transform The original transform
---@param point vector The point around which to rotate
---@param rotation quaternion The rotation to apply
---@return transform rotated_transform The rotated transform
function AutoTransformRotateAroundPoint(transform, point, rotation)
	local transform_relative_to_point = Transform(VecSub(transform.pos, point), transform.rot)
	local rotated_transform = TransformToParentTransform(Transform(Vec(), rotation), transform_relative_to_point)
	rotated_transform.pos = VecAdd(rotated_transform.pos, point)
	
	return rotated_transform
end

---Equivalent to `{ GetQuatEuler(quat) }`
---@param quat quaternion
---@return vector
function AutoEulerTable(quat)
	return { GetQuatEuler(quat) }
end

---Returns a Vector for easy use when put into a parameter for xml
---@param vec any
---@param round number
---@return string
function AutoVecToXML(vec, round)
	round = AutoDefault(round, 0)
	return AutoRound(vec[1], round) .. ' ' .. AutoRound(vec[2], round) .. ' ' .. AutoRound(vec[3], round)
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

---Converts a string to be capitalized following the Camel Case pattern
---@param str string
---@return string
function AutoCamelCase(str)
	local subbed = str:gsub('_', ' ')
	return string.gsub(" " .. subbed, "%W%l", string.upper):sub(2)
end

---Returns 3 values from HSV color space from RGB color space
---@param hue number? The hue from 0 to 1
---@param sat number? The saturation from 0 to 1
---@param val number? The value from 0 to 1
---@return number, number, number rgb Returns the red, green, blue of the given hue, saturation, value
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
---@param r number? The red from 0 to 1
---@param g number? The green from 0 to 1
---@param b number? The blue from 0 to 1
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

---Converts a hex code or a table of hex codes to RGB color space
---@param hex string|table<string>
---@return number|table
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

---Converts an RGB color code or a table of RGB color codes to hexadecimal color space
---@param r number|table<number> Red component (0-1) or table of RGB color codes
---@param g number Green component (0-1) (optional)
---@param b number Blue component (0-1) (optional)
---@return string|table<string> Hexadecimal color code or table of hex codes
function AutoRGBtoHEX(r, g, b)
	local function f(x)
		local hx = string.format("%02X", math.floor(x * 255))
		return hx
	end
	
	if type(r) == 'table' then
		local t = {}
		for key, val in pairs(r) do
			local hex = "#" .. f(val[1]) .. f(val[2]) .. f(val[3])
			t[key] = hex
		end
		return t
	else
		local hex = "#" .. f(r) .. f(g) .. f(b)
		return hex
	end
end

---Performs `:byte()` on each character of a given string
---@param str string
---@return table<number>
function AutoStringToByteTable(str)
	local t = {}
	for i = 1, #str do
		t[i] = (str:sub(i, i)):byte()
	end
	return t
end

---Performs `:char()` on each number of a given table, returning a string
---
---The inverse of AutoStringToByteTable
---@param t table<number>
---@return string
function AutoByteTableToString(t)
	local str = ''
	for i, b in ipairs(t) do
		str = str .. string.char(b)
	end
	return str
end

--#endregion
--#region Game Functions

---Usually, the Primary Menu Button only is suppose to work in the mod's level, this is a work around to have it work in any level.
---@param title string
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
---@param t table<entity_handle>
---@param CheckIfValid boolean?
---@return table<{handle:entity_handle, type:entity_type, valid:boolean}>
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

function AutoGetTags(handle)
	local T = {}

	for _, t in pairs(ListTags(handle)) do
		T[t] = GetTagValue(handle, t)
	end
	
	return T
end

function AutoSetTags(handle, tags)
	for t, v in pairs(tags) do
		SetTag(handle, t, v)
	end
end


---Creates a list from a table of entity handles, containing the handle and it's type. If the handle is invalid then the type will be false.
---@param t table<entity_handle>
---@return table<{handle:entity_handle, type:entity_type}>
function AutoListHandleTypes(t)
	local nt = {}
	for key, value in pairs(t) do
		nt[key] = { handle = value, type = IsHandleValid(value) and GetEntityType(value) or false, tags = AutoGetTags(value) }
	end
	return nt
end

---Spawn in a script node in the game world.
---@param path td_path
---@param ... string|number?
---@return script_handle
function AutoSpawnScript(path, ...)
	local f = [[<script file="%s" param0="%s" param1="%s" param2="%s" param3="%s"/>]]
	local param = { arg[1] or '', arg[2] or '', arg[3] or '', arg[4] or '' }
	return Spawn((f):format(path, unpack(param)), Transform())[1]
end

---Spawn in a voxscript node in the game world. No parameters
---@param path td_path
---@return script_handle
function AutoSpawnVoxScript(path)
	local f = [[<voxscript file="%s"/>]]
	return Spawn((f):format(path), Transform())[1]
end

---Attempts to get the handle of the current script by abusing pause menu item keys
---
---May not work if a pause menu button is already being created from the script
---
---Original coded from Thomasims
---@return script_handle
function AutoGetScriptHandle()
	local id = tostring(math.random())
	PauseMenuButton(id)
	for _, handle in ipairs(ListKeys("game.pausemenu.items")) do
		local p = "game.pausemenu.items." .. handle
		if GetString(p) == id then
			ClearKey(p)
			return tonumber(handle)
		end
	end

	return 0 -- Compatability for scripts run in a ui environment like Command('game.startui')
end

---A Wrapper for QueryRaycast; comes with some extra features.
---@param origin vector
---@param direction vector
---@param maxDist number
---@param radius number?
---@param rejectTransparent boolean?
---@return { hit:boolean, intersection:vector, dist:number, normal:vector, shape:shape_handle, body:body_handle, dir:vector, dot:number, reflection:vector }
function AutoRaycast(origin, direction, maxDist, radius, rejectTransparent)
	direction = direction and VecNormalize(direction) or nil
	
	local data = {}
	data.hit, data.dist, data.normal, data.shape = QueryRaycast(origin, direction, maxDist or 256, radius, rejectTransparent)
	data.intersection = VecAdd(origin, VecScale(direction, data.dist))
	data.dir = direction
	data.dot = VecDot(direction, data.normal)
	data.reflection = VecSub(direction, VecScale(data.normal, data.dot * 2))
	data.body = GetShapeBody(data.shape)
	
	return data
end

---AutoRaycast from point A to point B. The distance will default to the distance between the points, but can be set.
---@param pointA vector
---@param pointB vector
---@param manualDistance number?
---@param radius number?
---@param rejectTransparent boolean?
---@return { hit:boolean, dist:number, normal:vector, shape:shape_handle, intersection:vector, body:body_handle, dir:vector, dot:number, reflection:vector }
function AutoRaycastTo(pointA, pointB, manualDistance, radius, rejectTransparent)
	local diff = VecSub(pointB, pointA)
	local distance_between_points = VecLength(diff)
	return AutoRaycast(pointA, diff, manualDistance or distance_between_points, radius, rejectTransparent), distance_between_points
end

---AutoRaycast using the view or player view as the origin and direction
---@param usePlayerCamera boolean
---@param maxDist number
---@param radius number?
---@param rejectTransparent boolean?
---@return { hit:boolean, dist:number, normal:vector, shape:shape_handle, intersection:vector, body:body_handle, dir:vector, dot:number, reflection:vector }
---@return transform cameraTransform
---@return vector cameraForward
function AutoRaycastCamera(usePlayerCamera, maxDist, radius, rejectTransparent)
	local trans = usePlayerCamera and GetPlayerCameraTransform() or GetCameraTransform()
	local fwd = AutoTransformFwd(trans)
	
	return AutoRaycast(trans.pos, fwd, maxDist, radius, rejectTransparent), trans, fwd
end

---A Wrapper for QueryClosestPoint; comes with some extra features.
---@param origin vector
---@param maxDist number
---@return { hit:boolean, point:vector, normal:vector, shape:shape_handle, body:body_handle, dist:number, dir:vector, dot:number, reflection:vector }
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
		data.body = GetShapeBody(data.shape)
	else
		data.dist = maxDist
	end
	
	return data
end

---A Wrapper for GetBodyClosestPoint; comes with some extra features.
---@param body body_handle
---@param origin vector
---@return { hit:boolean, point:vector, normal:vector, shape:shape_handle, body:body_handle, dist:number, dir:vector, dot:number, reflection:vector }
function AutoQueryClosestBody(body, origin)
	local data = {}
	data.hit, data.point, data.normal, data.shape = GetBodyClosestPoint(body, origin)
	
	if data.hit then
		local diff = VecSub(data.point, origin)
		local dir = VecNormalize(diff)
		local dot = VecDot(dir, data.normal)
		
		data.dist = VecLength(diff)
		data.dir = dir
		data.dot = dot
		data.reflection = VecSub(dir, VecScale(data.normal, 2 * dot))
		data.body = GetShapeBody(data.shape)
	end
	
	return data
end

---A Wrapper for GetShapeClosestPoint; comes with some extra features.
---@param shape shape_handle
---@param origin vector
---@return { hit:boolean, point:vector, normal:vector, dist:number, dir:vector, dot:number, reflection:vector, shape:shape_handle, body:body_handle }
function AutoQueryClosestShape(shape, origin)
	local data = {}
	data.hit, data.point, data.normal = GetShapeClosestPoint(shape, origin)
	
	if data.hit then
		local diff = VecSub(data.point, origin)
		local dir = VecNormalize(diff)
		local dot = VecDot(dir, data.normal)
		
		data.dist = VecLength(diff)
		data.dir = dir
		data.dot = dot
		data.reflection = VecSub(dir, VecScale(data.normal, 2 * dot))
		data.shape = shape
		data.body = GetShapeBody(shape)
	end
	
	return data
end

---Unsteps a query from QueryClosestPoint
---
---Thank you Dima and Thomasims.
---@param shape shape_handle
---@param query_origin vector
---@param voxel_stepped_point vector
---@return vector
function AutoResolveUnsteppedPoint(shape, query_origin, voxel_stepped_point)
	local shape_world_transform = GetShapeWorldTransform(shape)
	local relative_query_origin = TransformToLocalPoint(shape_world_transform, query_origin)
	local relative_query_point = TransformToLocalPoint(shape_world_transform, voxel_stepped_point)

	local _, _, _, scale = GetShapeSize(shape)
	local offset = AutoVecOne(scale / 2)

	local applied_offset = AutoVecClampVec(relative_query_origin, VecSub(relative_query_point, offset), VecAdd(relative_query_point, offset))
	return TransformToParentPoint(shape_world_transform, applied_offset) -- Extrapolated
end

---@param shapes_a shape_handle[]
---@param shapes_b shape_handle[]
---@return false|{ [1]:shape_handle, [2]:shape_handle }
function AutoIsShapesTouchingShapes(shapes_a, shapes_b)
	for i=1, #shapes_a do
		local sa = shapes_a[i]
		for j=1, #shapes_b do
			local sb = shapes_b[j]
			if sa ~= sb and IsShapeTouching(sa, sb) then return { sa, sb } end
		end
	end

	return false
end

---Goes through each shape on a body and adds up their voxel count
---@param body body_handle
---@return integer
function AutoGetBodyVoxels(body)
	local v = 0
	for _, s in pairs(GetBodyShapes(body)) do
		v = v + GetShapeVoxelCount(s)
	end
	return v
end

---Scales the velocity of a body by `scale`
---@param body body_handle
---@param scale number
---@return vector scaled
---@return vector orginal
function AutoScaleBodyVelocity(body, scale)
	local orginal = GetBodyVelocity(body)
	local scaled = VecScale(orginal, scale)
	SetBodyVelocity(body, scaled)
	return scaled, orginal
end

---Scales the angular velocity of a body by `scale`
---@param body body_handle
---@param scale number
---@return vector scaled
---@return vector orginal
function AutoScaleBodyAngularVelocity(body, scale)
	local current = GetBodyAngularVelocity(body)
	local scaled = VecScale(current, scale)
	SetBodyAngularVelocity(body, scaled)
	return scaled, current
end

---Gets the angle from a point to the forward direction of a transform
---@param point vector
---@param fromtrans transform
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
---@param from_transform transform? The Transform acting as the view, Default is the Player's Camera
---@param angle number? The Angle at which the point can be seen from, Default is the Player's FOV set in the options menu
---@param raycastcheck boolean? Check to make sure that the point is not obscured, Default is true
---@return boolean seen If the point is in View
---@return number? angle The Angle the point is away from the center of the looking direction
---@return number? distance The Distance from the point to fromtrans
function AutoPointInView(point, from_transform, angle, raycastcheck, raycasterror)
	from_transform = AutoDefault(from_transform, GetCameraTransform())
	angle = AutoDefault(angle, GetInt('options.gfx.fov'))
	raycastcheck = AutoDefault(raycastcheck, true)
	raycasterror = AutoDefault(raycasterror, 0)
	
	local useangle = not (angle < 0)
	
	local fromtopointdir = VecNormalize(VecSub(point, from_transform.pos))
	local fromdir = AutoTransformFwd(from_transform)
	
	local dot = VecDot(fromtopointdir, fromdir)
	local dotangle = math.deg(math.acos(dot))
	local seen = dotangle < angle / 2
	
	seen = (not useangle) and (true) or (seen)
	
	local distance = AutoVecDist(point, from_transform.pos)

	if seen then
		if raycastcheck then
			local hit, hitdist = QueryRaycast(from_transform.pos, fromtopointdir, distance, 0, true)
			if hit then
				if raycasterror > 0 then
					local hitpoint = VecAdd(from_transform.pos, VecScale(fromtopointdir, hitdist))
					if AutoVecDist(hitpoint, point) > raycasterror then
						seen = false
					end
				else
					seen = false
				end
			end
		end
	end
	
	return seen, dotangle, distance
end

---Gets the direction the player is inputting and creates a vector.
---
---`{ horizontal, 0, vertical }`
---@param length number?
---@return vector
function AutoPlayerInputDir(length)
	return VecScale({
		-InputValue('left') + InputValue('right'),
		0,
		-InputValue('up') + InputValue('down'),
	}, length or 1)
end

---Get the last Path Query as a path of points
---@param precision number The Accuracy
---@return table<vector>
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
---@param bodies body_handle[]
function AutoQueryRejectBodies(bodies)
	for _, h in pairs(bodies) do
		if h then
			QueryRejectBody(h)
		end
	end
end

---Reject a table of shapes for the next Query
---@param shapes shape_handle[]
function AutoQueryRejectShapes(shapes)
	for _, h in pairs(shapes) do
		if h then
			QueryRejectShape(h)
		end
	end
end

---Finds and rejects all shapes that do not have a given tag
---@param tag string
function AutoRejectShapesWithoutTag(tag, global)
	local all = FindShapes(nil, global)
	local keepers = FindShapes(tag, global)

	local last_handle = keepers[#keepers]
	for i=1, #all do
		local h = all[i]
		if h > last_handle or not AutoTableContains(keepers, h) then
			QueryRejectShape(h)
		end
	end
end

---Set the collision filter for the shapes owned by a body
---@param body body_handle
---@param layer number
---@param masknummber number bitmask
function AutoSetBodyCollisionFilter(body, layer, masknummber)
	local shapes = GetBodyShapes(body)
	for i in pairs(shapes) do
		SetShapeCollisionFilter(shapes[i], layer, masknummber)
	end
end

---Get the Center of Mass of a body in World space
---@param body body_handle
---@return vector
function AutoWorldCenterOfMass(body)
	local trans = GetBodyTransform(body)
	local pos = TransformToParentPoint(trans, GetBodyCenterOfMass(body))
	return pos
end

---Adds the velocity and angualr velocity of a body
---@param body body_handle
---@return number
function AutoSpeed(body)
	return VecLength(GetBodyVelocity(body)) + VecLength(GetBodyAngularVelocity(body))
end

---Attempt to predict the position of a body in time
---@param body body_handle
---@param time number
---@param raycast boolean? Check and Halt on Collision, Default is false
---@param funcbefore function?
---@return table<vector> log
---@return vector vel
---@return vector normal
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
---@param raycast boolean? Check and Halt on Collision, Default is false
---@return table<vector> log
---@return vector vel
---@return vector normal
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

--#endregion
--#region Shape Utility

function AutoWorldToShapeVoxelIndex(shape, world_point)
	local shape_size = { GetShapeSize(shape) }
	local shape_transform = GetShapeWorldTransform(shape)
	local shape_pos = TransformToLocalPoint(shape_transform, world_point)
	return AutoVecFloor(VecScale(shape_pos, 1 / shape_size[4]))
end

function AutoMoveShapeToNewBody(shape, static)
	local shape_transform = GetShapeWorldTransform(shape)
	local shape_body = GetShapeBody(shape)

	local shape_center = AutoShapeCenter(shape)
	local body_transform = Transform(shape_center, shape_transform.rot)
	local body = Spawn('<body/>', body_transform)[1]
	SetShapeBody(shape, body, TransformToLocalTransform(body_transform, GetShapeWorldTransform(shape)))

	if static then
		SetBodyDynamic(body, false)
	else
		local shape_body_velocity = { linear = GetBodyVelocityAtPos(shape_body, shape_center), rotational = GetBodyAngularVelocity(shape_body) }

		SetBodyDynamic(body, true)
		SetBodyActive(body, true)
		
		SetBodyVelocity(body, shape_body_velocity.linear)
		SetBodyAngularVelocity(body, shape_body_velocity.rotational)
	end
		
	return body
end

---SplitShape with some extra stuff to put each shape under a dynamic body - copying the velocity of the original shape.
---@param shape shape_handle
---@param removeResidual boolean?
---@param static boolean?
---@return body_handle[]
function AutoSplitShapeIntoBodies(shape, removeResidual, static)
	local new_bodies = {}

	for _, new_shape in pairs(SplitShape(shape, removeResidual)) do
		new_bodies[#new_bodies+1] = AutoMoveShapeToNewBody(new_shape, static)
	end

	return new_bodies
end

---Creates a new shape offset a 1x1x1 voxel in place of an existing voxel.
---@param shape shape_handle
---@param voxel_position vector
---@param keep_original boolean?
---@param no_body boolean?
---@param reject_materials material[]?
---@return body_handle|false
---@return shape_handle|false
function AutoPopVoxel(shape, voxel_position, keep_original, no_body, reject_materials)
	local material = { GetShapeMaterialAtIndex(shape, unpack(voxel_position)) }
	if material[1] == '' or (reject_materials and AutoTableContains(reject_materials, material[1])) then return false, false end

	local shape_size = { GetShapeSize(shape) }
	local shape_transform = GetShapeWorldTransform(shape)
	local shape_body = GetShapeBody(shape)
	local shape_body_velocity = { linear = GetBodyVelocity(shape_body), rotational = GetBodyAngularVelocity(shape_body) }

	if not keep_original then
		SetBrush('cube', 1, 0)
		DrawShapeLine(shape, voxel_position[1], voxel_position[2], voxel_position[3], voxel_position[1], voxel_position[2], voxel_position[3])
	end

	local body
	local new_shape

	if no_body then
		new_shape = CreateShape(GetWorldBody(), Transform(TransformToParentPoint(shape_transform, VecScale(voxel_position, shape_size[4])), shape_transform.rot), shape)
		SetBrush('cube', 1, material[6])
		DrawShapeLine(new_shape, 0, 0, 0, 0, 0, 0)
	else
		local body_transform = Transform(TransformToParentPoint(shape_transform, VecScale(voxel_position, shape_size[4])), shape_transform.rot)
		body = Spawn('<body/>', body_transform)[1]
	
		new_shape = CreateShape(body, Transform(), shape)
		SetBrush('cube', 1, material[6])
		DrawShapeLine(new_shape, 0, 0, 0, 0, 0, 0)
	
		SetBodyDynamic(body, true)
		SetBodyActive(body, true)
	
		SetBodyVelocity(body, shape_body_velocity.linear)
		SetBodyAngularVelocity(body, shape_body_velocity.rotational)
	end

	return body, new_shape
end

---A function inspired by the Liquify Mod.
---@param shape shape_handle
---@param keep_original boolean?
---@param inherit_tags boolean?
---@param no_bodies boolean?
---@return body_handle[]
---@return shape_handle[]
function AutoLiquifyShape(shape, keep_original, inherit_tags, no_bodies)
	local shape_size = { GetShapeSize(shape) }

	local bodies = {}
	local shapes = {}
				
	for z=0, shape_size[3] - 1 do
		for y=0, shape_size[2] - 1 do
			for x=0, shape_size[1] - 1 do
				local new_body, new_shape = AutoPopVoxel(shape, { x, y, z }, true, no_bodies)

				if new_body then bodies[#bodies+1] = new_body end
				if new_shape then shapes[#shapes+1] = new_shape end
			end
		end
	end

	if inherit_tags then
		local parent_body_tags = AutoGetTags(GetShapeBody(shape))
		local parent_shape_tags = AutoGetTags(shape)

		for _, b in pairs(bodies) do
			AutoSetTags(b, parent_body_tags)
			
			for _, s in pairs(GetBodyShapes(b)) do
				AutoSetTags(s, parent_shape_tags)
			end
		end
	end

	if not keep_original then
		Delete(shape)
	end

	return bodies, shapes
end

---@param shape shape_handle
---@param world_point vector
---@param inner_radius number
---@param outer_radius number
---@param pop_voxels boolean
---@param pop_reject_materials any
---@param pop_voxels_inherit_tags boolean
---@param pop_voxels_no_bodies boolean
---@return body_handle[]
---@return shape_handle[]
function AutoCarveSphere(shape, world_point, inner_radius, outer_radius, pop_voxels, pop_reject_materials, pop_voxels_inherit_tags, pop_voxels_no_bodies)
	local popped_bodies = {}
	local popped_shapes = {}

	local function action(voxel_position)
		if pop_voxels then
			local new_body, new_shape = AutoPopVoxel(shape, voxel_position, false, pop_voxels_no_bodies, pop_reject_materials)

			if new_body then popped_bodies[#popped_bodies+1] = new_body end
			if new_shape then popped_shapes[#popped_shapes+1] = new_shape end
		else
			local material = { GetShapeMaterialAtIndex(shape, unpack(voxel_position)) }
			if material[1] == '' then return false end

			SetBrush('cube', 1, 0)
			DrawShapeLine(shape, voxel_position[1], voxel_position[2], voxel_position[3], voxel_position[1], voxel_position[2], voxel_position[3])
		end
	end

	local x, y, z = unpack(AutoWorldToShapeVoxelIndex(shape, world_point))
	local inner_radius_squared = inner_radius ^ 2
	local outer_radius_squared = outer_radius ^ 2

	for i = math.floor(x - outer_radius), math.ceil(x + outer_radius) do
		for j = math.floor(y - outer_radius), math.ceil(y + outer_radius) do
			for k = math.floor(z - outer_radius), math.ceil(z + outer_radius) do
				local dx = i - x
				local dy = j - y
				local dz = k - z

				local distanceSquared = dx * dx + dy * dy + dz * dz

				if distanceSquared <= outer_radius_squared and distanceSquared >= inner_radius_squared then
					action({ i, j, k })
				end
			end
		end
	end

	if pop_voxels and pop_voxels_inherit_tags then
		local parent_body_tags = AutoGetTags(GetShapeBody(shape))
		local parent_shape_tags = AutoGetTags(shape)

		for _, b in pairs(popped_bodies) do
			AutoSetTags(b, parent_body_tags)
			
			for _, s in pairs(GetBodyShapes(b)) do
				AutoSetTags(s, parent_shape_tags)
			end
		end
	end

	return popped_bodies, popped_shapes
end

--#endregion
--#region Environment

---@class environment
---@field ambience { [1]:td_path, [2]:number }
---@field ambient number
---@field ambientexponent number
---@field brightness number
---@field constant { [1]:number, [2]:number, [3]:number }
---@field exposure { [1]:number, [2]:number }
---@field fogcolor { [1]:number, [2]:number, [3]:number }
---@field fogparams { [1]:number, [2]:number, [3]:number, [4]:number }
---@field fogscale number
---@field nightlight boolean
---@field puddleamount number
---@field puddlesize number
---@field rain number
---@field skybox td_path
---@field skyboxbrightness number
---@field skyboxrot number
---@field skyboxtint { [1]:number, [2]:number, [3]:number }
---@field slippery number
---@field snowamount { [1]:number, [2]:number }
---@field snowdir { [1]:number, [2]:number, [3]:number, [4]:number }
---@field snowonground boolean
---@field sunbrightness number
---@field suncolortint { [1]:number, [2]:number, [3]:number }
---@field sundir { [1]:number, [2]:number, [3]:number }|'auto'|0
---@field sunfogscale number
---@field sunglare number
---@field sunlength number
---@field sunspread number
---@field waterhurt number
---@field wetness number
---@field wind { [1]:number, [2]:number, [3]:number }

---@type environment_property

---Returns a table of every property of the current environment
---@return environment
function AutoGetEnvironment()
	local params = {
		"ambience",
		"ambient",
		"ambientexponent",
		"brightness",
		"constant",
		"exposure",
		"fogcolor",
		"fogparams",
		"fogscale",
		"nightlight",
		"puddleamount",
		"puddlesize",
		"rain",
		"skybox",
		"skyboxbrightness",
		"skyboxrot",
		"skyboxtint",
		"slippery",
		-- "snowamount", -- SCREWS WITH THINGS IN LATEST RELEASE
		-- "snowdir", -- SCREWS WITH THINGS IN LATEST RELEASE
		-- "snowonground", -- SCREWS WITH THINGS IN LATEST RELEASE
		"sunbrightness",
		"suncolortint",
		"sundir",
		"sunfogscale",
		"sunglare",
		"sunlength",
		"sunspread",
		"waterhurt",
		"wetness",
		"wind",
	}
	
	local assembled = {}
	for _, k in ipairs(params) do
		local get = { GetEnvironmentProperty(k) }
		assembled[k] = #get > 1 and get or get[1]
	end
	
	return assembled
end

---Sets every environment property of AutoGetEnvironment
---@param Environment environment
function AutoSetEnvironment(Environment)
	for k, v in pairs(Environment) do
		if type(v) == 'table' then
			SetEnvironmentProperty(k, unpack(v))
		else
			SetEnvironmentProperty(k, v)
		end
	end
end

---Draws Sprites around the view to provide the illusion of a flat background
---@param r number
---@param g number
---@param b number
---@param a number
---@param sprite sprite_handle? Defaults to TD's 'ui/menu/white-32.png'
function AutoFlatBackground(r, g, b, a, sprite, distance)
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

---Returns and environemnt that eliminates as much lighting as possible, making colors look flat.
---
---Requires a flat DDS file.
---@param pathToDDS td_path
---@return environment
function AutoFlatEnvironment(pathToDDS)
	return {
		ambience = { "outdoor/field.ogg", 0 },
		ambient = { 1 },
		ambientexponent = { 10 ^ -37.9275 },
		brightness = { 1 },
		constant = { 0, 0, 0 },
		exposure = { 1, 1 },
		fogColor = { 0, 0, 0 },
		fogParams = { 0, 0, 0, 0 },
		fogscale = { 0 },
		nightlight = { true },
		puddleamount = { 0 },
		puddlesize = { 0 },
		rain = { 0 },
		skybox = { pathToDDS },
		skyboxbrightness = { 1 },
		skyboxrot = { 0 },
		skyboxtint = { 1, 1, 1 },
		slippery = { 0 },
		snowamount = { 0, 0 },
		snowdir = { 0, 0, 1, 0 },
		snowonground = {},
		sunBrightness = { 0 },
		sunColorTint = { 0, 0, 0 },
		sunDir = { 0, 0, 0 },
		sunFogScale = { 0 },
		sunGlare = { 0 },
		sunLength = { 0 },
		sunSpread = { 0 },
		waterhurt = { 0 },
		wetness = { 0 },
		wind = { 0, 0, 0 },
	}
end

--#endregion
--#region Post Processing

---@class postprocessing
---@field saturation number
---@field colorbalance { [1]:number, [2]:number, [3]:number }
---@field brightness number
---@field gamma number
---@field bloom number

---Returns a table of every property of the current post-processing
---@return postprocessing
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
		local get = { GetPostProcessingProperty(k) }
		assembled[k] = #get > 1 and get or get[1]
	end
	
	return assembled
end

---Sets every post-processing property of AutoGetPostProcessing
---@param PostProcessing postprocessing
function AutoSetPostProcessing(PostProcessing)
	for k, v in pairs(PostProcessing) do
		if type(v) == 'table' then
			SetPostProcessingProperty(k, unpack(v))
		else
			SetPostProcessingProperty(k, v)
		end
	end
end

--#endregion
--#region Debug

--- Returns the current Line Number.
---
--- This function is adapted from the UMF Framework
---
--- https://github.com/Thomasims/TeardownUMF/blob/master/src/util/debug.lua
---@param level integer? Optional
---@return integer
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
---@param level integer? Optional
---@return string?
function AutoGetStackTrace(level)
	level = (level or 0)
	local _, line = pcall(error, '', level + 3) -- The level + 3 is to get out of error, then out of pcall, then out of this function
	return line
end

---Creates a neatly formatted string given any value of any type, including tables
---@param t any
---@param floating_point number|false?
---@param singleline_at number?
---@param show_number_keys boolean?
---@param ignore_keys any[]?
---@param lua_compatible boolean?
---@param indents number?
---@param visited_tables table?
---@return string
function AutoToString(t, floating_point, singleline_at, show_number_keys, ignore_keys, lua_compatible, indents, visited_tables)
	singleline_at = singleline_at or 1
	indents = indents or 0
	visited_tables = visited_tables or {}
	
	local indent_str = '  '
	local str = ""
	
	if type(t) ~= "table" then
		if type(t) == "string" then
			return string.format("%q", t)
		elseif type(t) == 'number' then
			if floating_point then
				return string.format('%.' .. tostring(floating_point) .. 'f', t)
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
		if not ignore_keys or not AutoTableContains(ignore_keys, k) then
			if not passedSLthreshold then
				str = str .. indent_str:rep(indents+1)
			end
			
			-- local k_str = not_a_number and tostring(k) or (show_number_keys and string.format('[%s]', tostring(k)) or false)
			-- local k_str = not_a_number and (lua_compatible and ("['%s']"):format(tostring(k))) or (show_number_keys and ("[%s]"):format(tostring(k)) and false)
			local k_str = false
			if type(k) ~= "number" then
				k_str = lua_compatible and ("['%s']"):format(tostring(k)) or tostring(k)
			elseif show_number_keys then
				k_str = ("[%s]"):format(tostring(k))
			end
			
			local prefix = k_str and k_str .. " = " or ''
			
			local v_str = AutoToString(v, floating_point, singleline_at - 1, show_number_keys, ignore_keys, lua_compatible, indents + 1, visited_tables)
			str = str .. prefix .. v_str .. ", "
			
			if not passedSLthreshold then
				str = str .. "\n"
			end
		end
	end
	str = (passedSLthreshold and str:sub(1, -3) or str) .. (passedSLthreshold and " }" or (indent_str:rep(indents) .. "}"))
	return str
end

---A Alternative to DebugPrint that uses AutoToString(), works with tables. Returns the value
---@param value any
---@param format_output string?
---@param floating_point number|false?
---@param singleline_at number?
---@param show_number_keys boolean?
---@param ignore_keys any[]?
---@param lua_compatible boolean?
---@return any
function AutoInspect(value, format_output, floating_point, singleline_at, show_number_keys, ignore_keys, lua_compatible)
	local text = AutoToString(value, floating_point, singleline_at or 3, show_number_keys, ignore_keys, lua_compatible)
	local formatted_text = format_output and (string.match(format_output, "%%s") ~= nil and string.format(format_output, text) or format_output .. text) or text
	local split = AutoSplit(formatted_text, '\n')
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
---@param singleline_at number?
---@param floating_point number|false?
---@param show_number_keys boolean?
---@param lua_compatible boolean?
---@param ignore_keys any[]?
---@return any
function AutoInspectConsole(value, format_output, floating_point, singleline_at, show_number_keys, ignore_keys, lua_compatible)
	local text = AutoToString(value, floating_point, singleline_at or 3, show_number_keys, ignore_keys, lua_compatible)
	local formatted_text = format_output and (string.match(format_output, "%%s") ~= nil and string.format(format_output, text) or format_output .. text) or text

	print(formatted_text)
	return value
end

---AutoInspect that prints to DebugWatch.
---
---Name will default to current line number
---@param value any
---@param name string?
---@param singleline_at number?
---@param floating_point number|false?
---@param show_number_keys boolean?
---@param lua_compatible boolean?
---@param ignore_keys any[]?
function AutoInspectWatch(value, name, floating_point, singleline_at, show_number_keys, ignore_keys, lua_compatible)
	if not name then name = 'Inspecting Line ' .. AutoGetCurrentLine(1) end
	DebugWatch(name, AutoToString(value, floating_point, singleline_at, show_number_keys, ignore_keys, lua_compatible))
	return value
end

---Prints 24 blank lines to quote on quote, "clear the console"
function AutoClearConsole()
	for i = 1, 24 do DebugPrint('') end
end

---Draws a Transform
---@generic T : transform|vector
---@param transform T
---@param size number? the size in meters, Default is 0.5
---@param alpha number? Default is 1
---@param draw boolean? Whether to use DebugLine or DrawLine, Default is false (DebugLine)
---@return T
function AutoDrawTransform(transform, size, alpha, draw)
	if not transform then return end
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

---Simply draws a box given a center and the half size.
---@param point vector
---@param halfextents number|vector
---@param r number
---@param g number
---@param b number
---@param a number
---@return vector aa lower bounds point
---@return vector bb upper point
function AutoDrawBox(point, halfextents, r, g, b, a)
	local aa, bb = AutoAABBBoxFromPoint(point, halfextents)
	AutoDrawAABB(aa, bb, r, g, b, a)
	
	return aa, bb
end

---Draws a Transform as a Cone
---@param transform transform
---@param sides number? the amount of sides on the cone, Default is 12
---@param angle number? how wide the cone is in degrees, Default is 25
---@param size number? the size in meters, Default is 0.5
---@param color table? Default is 1
---@param draw boolean? Whether to use DebugLine or DrawLine, Default is false (DebugLine)
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

--#endregion
--#region Graphing

AutoGraphs = {}

---Creates a Continuous Graph that can be drawn. The given value is added into the graph as the previous ones are kept in memory.
---@param id string
---@param value number
---@param range number? Default is 64
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
---@param rangemin number? Default is 0
---@param rangemax number? Default is 1
---@param func function? Is fed one parameter, a number ranging from rangemin to rangemax, Defaults to a Logisitc Function
---@param steps number? How many steps, or the interval of values taken from the range.
function AutoGraphFunction(id, rangemin, rangemax, func, steps)
	rangemin = AutoDefault(rangemin, 0)
	rangemax = AutoDefault(rangemax, 1)
	steps = AutoDefault(steps, 100)
	
	func = AutoDefault(func, function(x)
		return AutoSigmoid(x, 1, -10, 0.5)
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
---@param rangemin number? If left nil, then the graph will automatically stretch the values to fill the bottom of the graph. Default is nil
---@param rangemax number? If left nil, then the graph will automatically stretch the values to fill the top of the graph. Default is nil
---@param linewidth number? The line width, Default is 2
function AutoGraphDraw(id, sizex, sizey, rangemin, rangemax, linewidth)
	local Graph = AutoGraphs[id]
	if Graph == nil then error("Graph Doesn't exist, nil") end
	
	sizex = AutoDefault(sizex, 128)
	sizey = AutoDefault(sizey, 64)
	
	UiPush()
	UiWindow(sizex + 8, sizey + 8, true)
	
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
			
			UiColor(1, 1, 1, 1)
			UiAlign('left top')
			UiRect(width, distance + 1)
			UiPop()
		end
	end

	UiPop()
end

--#endregion
--#region Registry

---Concats any amount of strings by adding a single period between them
---@vararg string
---@return string
function AutoKey(...)
	return table.concat(arg, '.')
end

---One out of the many methods to convert a registry key to a table
---@param key string
---@return table
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

---Gets a Int from the registry, if the key does not exist, then set the key to the default value and return it.
---@param path string
---@param default integer
---@return integer
function AutoKeyDefaultInt(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetInt(path, default)
	else
		SetInt(path, default)
		return default
	end
end

---Gets a Float from the registry, if the key does not exist, then set the key to the default value and return it.
---@param path string
---@param default number
---@return number
function AutoKeyDefaultFloat(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetFloat(path, default)
	else
		SetFloat(path, default)
		return default
	end
end

---Gets a String from the registry, if the key does not exist, then set the key to the default value and return it.
---@param path string
---@param default string
---@return string
function AutoKeyDefaultString(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetString(path, default)
	else
		SetString(path, default)
		return default
	end
end

---Gets a Bool from the registry, if the key does not exist, then set the key to the default value and return it.
---@param path string
---@param default boolean
---@return boolean
function AutoKeyDefaultBool(path, default)
	if path == nil then error("path nil") end
	if HasKey(path) then
		return GetBool(path)
	else
		SetBool(path, default)
		return default
	end
end

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

---Attempts to create a table that when written to, will update the registry, and when read from, will pull from the registry
---@param path string
---@return table
function AutoRegistryBindedTable(path)
	local t = {}
	t.__path = path
	setmetatable(t, RegistryTableMeta)
	
	return t
end

--#endregion
--#region User Interface

---Corrects a 0 to 1 screen-space coordinate for Teardown's barrel distortion
---@param texture_coord { [1]:number, [2]:number }
---@param amount number 1.0 as default 
---@return { [1]:number, [2]:number }
function AutoCorrectBarrelDistortion(texture_coord, amount)
	local tc = { texture_coord[1], texture_coord[2] }

	local r = math.sqrt(tc[1] * tc[1] + tc[2] * tc[2])
	local r2 = r * (0.94 + 0.04 * r * r)
	local norm = math.sqrt(tc[1] * tc[1] + tc[2] * tc[2])

	if norm > 0 then
		tc = {
			tc[1] / norm * r2,
			tc[2] / norm * r2,
		}
	end

	return VecLerp(texture_coord, tc, -amount)
end

---@param local_point vector
---@param camera_fov number
---@param ui_width number
---@param barrel_distortion number?
---@return vector
function AutoLocalSpaceToUiSpace(local_point, camera_fov, ui_width, barrel_distortion)
	local rad_fov = math.rad(camera_fov)
	local focal_length = ui_width / (2 * math.tan(rad_fov / 2))
	local ui_space = VecScale(local_point, -focal_length / local_point[3])

	ui_space[2] = -ui_space[2]

	if barrel_distortion and barrel_distortion ~= 0 then
		-- Normalize UI space coordinates to [-1, 1]
		local half_width = ui_width / 2
		local norm_ui_space = {
			AutoMap(ui_space[1], -half_width, half_width, -1, 1),
			AutoMap(ui_space[2], -half_width, half_width, -1, 1),
		}
		
		-- Apply inverse barrel distortion
		norm_ui_space = AutoCorrectBarrelDistortion(norm_ui_space, barrel_distortion)
	
		-- Convert normalized coordinates back to UI space
		ui_space = {
			AutoMap(norm_ui_space[1], -1, 1, -half_width, half_width),
			AutoMap(norm_ui_space[2], -1, 1, -half_width, half_width),
			ui_space[3]
		}
	end

	return ui_space
end

---@param origin vector
---@param direction vector
---@param plane_distance number
---@return vector UiCoordinates
function AutoRaycastToUiPlane(origin, direction, plane_distance, camera_fov, ui_width, barrel_distortion)
	local ui_plane = Transform(
		Vec(0, 0, -plane_distance),
		Quat()
	) --[[@as plane]]

	local ray = AutoRaycastPlane(ui_plane, origin, direction)

	return AutoLocalSpaceToUiSpace(ray.intersection, camera_fov, ui_width, barrel_distortion)
end

--- Calculates the radius in screen space of a cone based on angle, view FOV, and UI width
---@param radians number Angle
---@param camera_fov number Camera field of view in degrees
---@param ui_width number Width of the UI element where the crosshair is displayed
---@return number Radius of the cone in screen space
function AutoUiCalculateConeRadius(radians, camera_fov, ui_width)
	-- Calculate the tangent of half the angle
	local tan_half_angle = math.tan(radians / 2)

	-- Convert view FOV from degrees to radians
	local vertical_fov_rad = math.rad(camera_fov)

	-- Calculate the radius in world space
	local radius_world = tan_half_angle / math.tan(vertical_fov_rad / 2)

	-- Convert the world space radius to screen space using UI width
	local radius_screen = radius_world * (ui_width / 2)

	return radius_screen
end

---UiTranslate and UiAlign to the Center
function AutoUiCenter()
	UiTranslate(UiCenter(), UiMiddle())
	UiAlign('center middle')
end

---Returns the bounds, optionally subtracted by some amount
---@param subtract number?
---@return number
---@return number
function AutoUiBounds(subtract)
	subtract = subtract or 0
	return UiWidth() - subtract, UiHeight() - subtract
end

---@param a table
---@param b table?
---@return number
function AutoUiDistance(a, b)
	b = b or { 0, 0 }
	return VecLength(VecSub(Vec(a[1], a[2]), Vec(b[1], b[2])))
end

---Draws a line between two points in screen space
---
---Relative to current cursor position
---@param p1 { [1]:number, [2]:number }
---@param p2 { [1]:number, [2]:number }
---@param width integer? Default is 2
function AutoUiLine(p1, p2, width)
	width = AutoDefault(width, 2)
	local angle = math.atan2(p2[1] - p1[1], p2[2] - p1[2]) * 180 / math.pi
	local distance = AutoVecDistNoZ(p1, p2)
	
	UiPush()
	UiTranslate(p1[1] - width / 2, p1[2] - width / 2)
	UiRotate(angle)
	
	UiAlign('center top')
	UiRect(width, distance + 1)
	UiPop()
end

---Draws a cricle out of lines in screen space
---
---Relative to current cursor position
---@param radius number
---@param width integer? Default is 2
---@param steps integer?
function AutoUiCircle(radius, width, steps)
	width = width or 2
	steps = steps or 16
	
	for i = 1, steps do
		local t1 = (i - 1) / steps * math.pi * 2
		local t2 = i / steps * math.pi * 2
		
		UiPush()
		
		UiTranslate(width/2, width/2)
		AutoUiLine({
			math.cos(t1) * radius,
			math.sin(t1) * radius
		}, {
			math.cos(t2) * radius,
			math.sin(t2) * radius
		}, width)
		
		UiPop()
	end
end

---Draws a Fancy looking Arrow between two points on the screen
---
---Relative to current cursor position
---@param p1 { [1]: number, [2]:number }
---@param p2 { [1]: number, [2]:number }
---@param line_width integer
---@param radius integer
function AutoUIArrow(p1, p2, line_width, radius)
	local dir = VecNormalize(VecSub(p2, p1))
	local angle = math.atan2(unpack(AutoSwizzle(dir, 'yx')))
	
	local dist = AutoVecDistNoZ(p1, p2)
	local arrow_scale = AutoSigmoid(dist, line_width * 3, -0.3, 15)
	
	UiPush()
	
	if radius > 0 then
		UiPush()
		UiTranslate(unpack(p1))
		AutoUiCircle(radius, line_width, 32)
		UiPop()
	end
	
	if dist > radius + line_width then
		AutoUiLine(AutoVecMove(p1, dir, radius), p2, line_width)
		
		if arrow_scale >= 0.1 then
			AutoUiLine(p2, {
				p2[1] + math.cos(angle + math.rad(135)) * arrow_scale,
				p2[2] + math.sin(angle + math.rad(135)) * arrow_scale
			}, line_width)
			AutoUiLine(p2, {
				p2[1] + math.cos(angle + math.rad(-135)) * arrow_scale,
				p2[2] + math.sin(angle + math.rad(-135)) * arrow_scale
			}, line_width)
		end
	end
	
	UiPop()
end

---Draws a Fancy looking Arrow between two points in the world
---
---Relative to current cursor position
---@param p1 vector
---@param p2 vector
---@param line_width integer
---@param radius integer
function AutoUIArrowInWorld(p1, p2, line_width, radius)
	local s_p1 = { UiWorldToPixel(p1) }
	local s_p2 = { UiWorldToPixel(p2) }
	
	if s_p1[3] > 0 or s_p2[3] > 0 then
		AutoUIArrow(s_p1, s_p2, line_width, s_p1[3] > 0 and radius or 0)
	end
end

---OLD
---UI
---FUNCTIONS

-- AutoPad = { none = 0, atom = 4, micro = 6, thin = 12, thick = 24, heavy = 48, beefy = 128 }

-- AutoPrimaryColor = { 0.95, 0.95, 0.95, 1 }
-- AutoSpecialColor = { 1, 1, 0.55, 1 }
-- AutoSecondaryColor = { 0, 0, 0, 0.55 }
-- AutoFont = 'regular.ttf'
-- local SpreadStack = {}

-- ---Draws some text at a world position.
-- ---@param text string|number? Text Displayed, Default is 'nil'
-- ---@param position vector The WorldSpace Position
-- ---@param occlude boolean? Hides the tooltip behind walls, Default is false
-- ---@param fontsize number? Fontsize, Default is 24
-- ---@param alpha number? Alpha, Default is 0.75
-- function AutoTooltip(text, position, occlude, fontsize, alpha)
-- 	text = AutoDefault(text or "nil")
-- 	occlude = AutoDefault(occlude or false)
-- 	fontsize = AutoDefault(fontsize or 24)
-- 	alpha = AutoDefault(alpha or 0.75)
-- 	bold = AutoDefault(bold or false)

-- 	if occlude then if not AutoPointInView(position, nil, nil, occlude) then return end end

-- 	UiPush()
-- 	UiAlign('center middle')
-- 	local x, y, dist = UiWorldToPixel(position)
-- 	if dist > 0 then
-- 		UiTranslate(x, y)
-- 		UiWordWrap(UiMiddle())

-- 		UiFont(AutoFont, fontsize)
-- 		UiColor(0, 0, 0, 0)
-- 		local rw, rh = UiText(text)

-- 		UiColorFilter(1, 1, 1, alpha)
-- 		UiColor(unpack(AutoSecondaryColor))
-- 		UiRect(rw, rh)

-- 		UiColor(unpack(AutoPrimaryColor))
-- 		UiText(text)
-- 		UiPop()
-- 	end
-- end

-- ---Takes an alignment and returns a Vector representation.
-- ---@param alignment string
-- ---@return table
-- function AutoAlignmentToPos(alignment)
-- 	str, y = 0, 0
-- 	if string.find(alignment, 'left') then str = -1 end
-- 	if string.find(alignment, 'center') then str = 0 end
-- 	if string.find(alignment, 'right') then str = 1 end
-- 	if string.find(alignment, 'bottom') then y = -1 end
-- 	if string.find(alignment, 'middle') then y = 0 end
-- 	if string.find(alignment, 'top') then y = 1 end
-- 	return { x = str, y = y }
-- end

-- ---The next Auto Ui functions will be spread Down until AutoSpreadEnd() is called
-- ---@param padding number? The amount of padding that will be used, Default is AutoPad.thin
-- function AutoSpreadDown(padding)
-- 	table.insert(SpreadStack, { type = 'spread', direction = 'down', padding = AutoDefault(padding, AutoPad.thin) })
-- 	UiPush()
-- end

-- ---The next Auto Ui functions will be spread Up until AutoSpreadEnd() is called
-- ---@param padding number? The amount of padding that will be used, Default is AutoPad.thin
-- function AutoSpreadUp(padding)
-- 	table.insert(SpreadStack, { type = 'spread', direction = 'up', padding = AutoDefault(padding, AutoPad.thin) })
-- 	UiPush()
-- end

-- ---The next Auto Ui functions will be spread Right until AutoSpreadEnd() is called
-- ---@param padding number? The amount of padding that will be used, Default is AutoPad.thin
-- function AutoSpreadRight(padding)
-- 	table.insert(SpreadStack, { type = 'spread', direction = 'right', padding = AutoDefault(padding, AutoPad.thin) })
-- 	UiPush()
-- end

-- ---The next Auto Ui functions will be spread Left until AutoSpreadEnd() is called
-- ---@param padding number? The amount of padding that will be used, Default is AutoPad.thin
-- function AutoSpreadLeft(padding)
-- 	table.insert(SpreadStack, { type = 'spread', direction = 'left', padding = AutoDefault(padding, AutoPad.thin) })
-- 	UiPush()
-- end

-- ---The next Auto Ui functions will be spread Verticlely across the Height of the Bounds until AutoSpreadEnd() is called
-- ---@param count number? The amount of Auto Ui functions until AutoSpreadEnd()
-- function AutoSpreadVerticle(count)
-- 	table.insert(SpreadStack, { type = 'spread', direction = 'verticle', length = UiHeight(), count = count })
-- 	UiPush()
-- end

-- ---The next Auto Ui functions will be spread Horizontally across the Width of the Bounds until AutoSpreadEnd() is called
-- ---@param count number? The amount of Auto Ui functions until AutoSpreadEnd()
-- function AutoSpreadHorizontal(count)
-- 	table.insert(SpreadStack, { type = 'spread', direction = 'horizontal', length = UiWidth(), count = count })
-- 	UiPush()
-- end

-- function AutoGetSpread()
-- 	local _l = 0
-- 	local count = AutoTableCount(SpreadStack)
-- 	if count <= 0 then return nil end
-- 	for i = count, 1, -1 do
-- 		if SpreadStack[i].type == 'spread' then
-- 			_l = _l + 1
-- 			if _l >= 1 then
-- 				return SpreadStack[i], _l
-- 			end
-- 		end
-- 	end
-- 	return nil
-- end

-- function AutoSetSpread(Spread)
-- 	local count = AutoTableCount(SpreadStack)
-- 	for i = count, 1, -1 do
-- 		if SpreadStack[i].type == 'spread' then
-- 			str = SpreadStack[i]
-- 		end
-- 	end

-- 	str = Spread
-- end

-- ---Stop the last known Spread
-- ---@return table a table with information about the transformations used
-- function AutoSpreadEnd()
-- 	local unitdata = { comb = { w = 0, h = 0 }, max = { w = 0, h = 0 } }
-- 	-- local _, LastSpread = AutoGetSpread(1)

-- 	while true do
-- 		local count = #SpreadStack

-- 		if SpreadStack[count].type ~= 'spread' then
-- 			if SpreadStack[count].data.rect then
-- 				local rect = SpreadStack[count].data.rect
-- 				unitdata.comb.w, unitdata.comb.h = unitdata.comb.w + rect.w, unitdata.comb.h + rect.h
-- 				unitdata.max.w, unitdata.max.h = math.max(unitdata.max.w, rect.w), math.max(unitdata.max.h, rect.h)
-- 			end

-- 			table.remove(SpreadStack, count)
-- 		else
-- 			UiPop()
-- 			table.remove(SpreadStack, count)

-- 			return unitdata
-- 		end
-- 		if count <= 0 then
-- 			return unitdata
-- 		end
-- 	end
-- end

-- function AutoHandleSpread(gs, data, type, spreadpad)
-- 	spreadpad = AutoDefault(spreadpad, false)

-- 	if not AutoGetSpread() then return end

-- 	if gs ~= nil then
-- 		if not spreadpad then pad = 0 else pad = gs.padding end
-- 		if gs.direction == 'down' then
-- 			UiTranslate(0, data.rect.h + pad)
-- 		elseif gs.direction == 'up' then
-- 			UiTranslate(0, -(data.rect.h + pad))
-- 		elseif gs.direction == 'right' then
-- 			UiTranslate(data.rect.w + pad, 0)
-- 		elseif gs.direction == 'left' then
-- 			UiTranslate(-(data.rect.w + pad), 0)
-- 		elseif gs.direction == 'verticle' then
-- 			UiTranslate(0, gs.length / gs.count * 1.5 + gs.length / gs.count)
-- 		elseif gs.direction == 'horizontal' then
-- 			UiTranslate(gs.length / gs.count, 0)
-- 		end
-- 	end

-- 	if type ~= nil then
-- 		table.insert(SpreadStack, { type = type, data = data })
-- 	end
-- end

-- ---Given the current string, will return a modified string based on the input of the user. It's basically just a text box. Has a few options.
-- ---@param current any
-- ---@param maxlength any
-- ---@param allowlowercase any
-- ---@param allowspecial any
-- ---@param forcekey any
-- ---@return any
-- ---@return any
-- ---@return boolean
-- function AutoTextInput(current, maxlength, allowlowercase, allowspecial, forcekey)
-- 	current = AutoDefault(current, '')
-- 	maxlength = AutoDefault(maxlength, 1 / 0)
-- 	allowlowercase = AutoDefault(allowlowercase, true)
-- 	allowspecial = AutoDefault(allowspecial, true)
-- 	forcekey = AutoDefault(forcekey, nil)

-- 	local modified = current

-- 	local special = {
-- 		['1'] = '!',
-- 		['2'] = '@',
-- 		['3'] = '#',
-- 		['4'] = '$',
-- 		['5'] = '%',
-- 		['6'] = '^',
-- 		['7'] = '&',
-- 		['8'] = '*',
-- 		['9'] = '(',
-- 		['0'] = ')',
-- 	}
-- 	local lpk = forcekey or InputLastPressedKey()

-- 	if lpk == 'backspace' then
-- 		modified = modified:sub(1, #modified - 1)
-- 	elseif lpk == 'delete' then
-- 		modified = ''
-- 	elseif #modified < maxlength then
-- 		if lpk == 'space' then
-- 			modified = modified .. ' '
-- 		elseif #lpk == 1 then
-- 			if not InputDown('shift') then
-- 				if allowlowercase then
-- 					lpk = lpk:lower()
-- 				end
-- 			else
-- 				if allowspecial and special[lpk] then
-- 					lpk = special[lpk]
-- 				end
-- 			end

-- 			modified = modified .. lpk
-- 		end
-- 	end

-- 	return modified, lpk ~= '' and lpk or nil, modified ~= current
-- end

-- -- local keys = {
-- -- 	"lmb", "mmb", "rmb", -- mouse
-- -- 	"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", -- numerical
-- -- 	"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
-- -- 	"y", "z", -- alphabatical
-- -- 	"f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12", -- function key
-- -- 	"uparrow", "downarrow", "leftarrow", "rightarrow", -- arrow key
-- -- 	"backspace", "alt", "delete", "home", "end", "pgup", "pgdown", "insert", "return", "space", "shift", "ctrl", "tab",
-- -- 	"esc", --random key
-- -- 	",", ".", "-", "+", -- undocumented key (yes, '=' key is '+' key)
-- -- }

-- -------------------------------------------------------------------------------------------------------------------------------------------------------
-- ----------------User Interface Creation Functions------------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------------------------------------------------------------

-- ---Create a Container with new bounds
-- ---@param width number
-- ---@param height number
-- ---@param padding number? The Amount of padding against sides of the container, Default is AutoPad.micro
-- ---@param clip boolean? Whether  to clip stuff outside of the container, Default is false
-- ---@param draw boolean? Draws the container's background, otherwise it will be invisible, Default is true
-- ---@return table containerdata
-- function AutoContainer(width, height, padding, clip, draw)
-- 	width = AutoDefault(width, 300)
-- 	height = AutoDefault(height, 400)
-- 	padding = math.max(AutoDefault(padding, AutoPad.micro), 0)
-- 	clip = AutoDefault(clip, false)
-- 	draw = AutoDefault(draw, true)

-- 	local paddingwidth = math.max(width - padding * 2, padding * 2)
-- 	local paddingheight = math.max(height - padding * 2, padding * 2)

-- 	UiWindow(width, height, clip)

-- 	UiAlign('left top')
-- 	if draw then
-- 		UiPush()
-- 		UiColor(unpack(AutoSecondaryColor))
-- 		UiImageBox("ui/common/box-solid-10.png", UiWidth(), UiHeight(), 10, 10)
-- 		UiPop()
-- 	end

-- 	hover = UiIsMouseInRect(UiWidth(), UiHeight())

-- 	UiTranslate(padding, padding)
-- 	UiWindow(paddingwidth, paddingheight, false)

-- 	local offset = { x = 0, y = 0 }

-- 	UiTranslate(offset.x, offset.y)

-- 	return { rect = { w = paddingwidth, h = paddingheight }, hover = hover }
-- end

-- ---Creates a Button
-- ---@param name string
-- ---@param fontsize number
-- ---@param paddingwidth number Amount of padding used Horizontally
-- ---@param paddingheight number Amount of padding used Vertically
-- ---@param draw boolean Draws the Button
-- ---@param spreadpad boolean Adds padding when used with AutoSpread...()
-- ---@return boolean Pressed
-- ---@return table ButtonData
-- function AutoButton(name, fontsize, color, paddingwidth, paddingheight, draw, spreadpad)
-- 	fontsize = AutoDefault(fontsize, 28)
-- 	color = AutoDefault(color, AutoPrimaryColor)
-- 	paddingwidth = AutoDefault(paddingwidth, AutoPad.thick)
-- 	paddingheight = AutoDefault(paddingheight, AutoPad.thin)
-- 	draw = AutoDefault(draw, true)
-- 	spreadpad = AutoDefault(spreadpad, true)

-- 	UiPush()
-- 	UiWordWrap(UiWidth() - AutoPad.thick)
-- 	UiFont(AutoFont, fontsize)
-- 	UiButtonHoverColor(unpack(AutoSpecialColor))
-- 	UiButtonPressColor(0.75, 0.75, 0.75, 1)
-- 	UiButtonPressDist(0.25)

-- 	UiColor(0, 0, 0, 0)
-- 	local rw, rh = UiText(name)
-- 	local padrw, padrh = rw + paddingwidth * 2, rh + paddingheight * 2

-- 	if draw then
-- 		hover = UiIsMouseInRect(padrw, padrh)
-- 		UiColor(unpack(color))

-- 		UiButtonImageBox('ui/common/box-outline-6.png', 6, 6, unpack(color))
-- 		pressed = UiTextButton(name, padrw, padrh)
-- 	end
-- 	UiPop()

-- 	local data = { pressed = pressed, hover = hover, rect = { w = padrw, h = padrh } }
-- 	if draw then AutoHandleSpread(AutoGetSpread(), data, 'draw', spreadpad) end

-- 	return pressed, data
-- end

-- ---Draws some Text
-- ---@param name string
-- ---@param fontsize number
-- ---@param draw boolean Draws the Text
-- ---@param spread boolean Adds padding when used with AutoSpread...()
-- ---@return table TextData
-- function AutoText(name, fontsize, color, draw, spread)
-- 	fontsize = AutoDefault(fontsize, 28)
-- 	draw = AutoDefault(draw, true)
-- 	spread = AutoDefault(spread, true)

-- 	UiPush()
-- 	UiWordWrap(UiWidth() - AutoPad.thick)
-- 	UiFont(AutoFont, fontsize)

-- 	UiColor(0, 0, 0, 0)
-- 	local rw, rh = UiText(name)

-- 	if draw then
-- 		UiPush()
-- 		UiWindow(rw, rh)
-- 		AutoCenter()

-- 		UiColor(unpack(color or AutoPrimaryColor))
-- 		UiText(name)
-- 		UiPop()
-- 	end
-- 	UiPop()

-- 	local data = { rect = { w = rw, h = rh }, hover = UiIsMouseInRect(rw, rh) }
-- 	if spread then AutoHandleSpread(AutoGetSpread(), data, 'draw', true) end

-- 	return data
-- end

-- ---Creates a Slider
-- ---@param set number The Current Value
-- ---@param min number The Minimum
-- ---@param max number The Maximum
-- ---@param lockincrement number The increment
-- ---@param paddingwidth Amount of padding used Horizontally
-- ---@param paddingheight Amount of padding used Vertically
-- ---@param spreadpad boolean Adds padding when used with AutoSpread...()
-- ---@return number NewValue
-- ---@return table SliderData
-- function AutoSlider(set, min, max, lockincrement, paddingwidth, paddingheight, spreadpad)
-- 	min = AutoDefault(min, 0)
-- 	max = AutoDefault(max, 1)
-- 	set = AutoDefault(set, min)
-- 	lockincrement = AutoDefault(lockincrement, 0)
-- 	paddingwidth = AutoDefault(paddingwidth, AutoPad.thick)
-- 	paddingheight = AutoDefault(paddingheight, AutoPad.micro)
-- 	spreadpad = AutoDefault(spreadpad, true)

-- 	local width = UiWidth() - paddingwidth * 2
-- 	local dotwidth, dotheight = UiGetImageSize("MOD/slider.png")

-- 	local screen = AutoMap(set, min, max, 0, width)

-- 	UiPush()
-- 	UiTranslate(paddingwidth, paddingheight)
-- 	UiColor(unpack(AutoSpecialColor))

-- 	UiPush()
-- 	UiTranslate(0, dotheight / 2)
-- 	UiRect(width, 2)
-- 	UiPop()

-- 	UiTranslate(-dotwidth / 2, 0)

-- 	screen, released = UiSlider('MOD/slider.png', "x", screen, 0, width)
-- 	screen = AutoMap(screen, 0, width, min, max)
-- 	screen = AutoRound(screen, lockincrement)
-- 	screen = AutoClamp(screen, min, max)
-- 	set = screen
-- 	UiPop()

-- 	local data = { value = set, released = released, rect = { w = width, h = paddingheight * 2 + dotheight } }
-- 	AutoHandleSpread(AutoGetSpread(), data, 'draw', spreadpad)

-- 	return set, data
-- end

-- ---Draws an Image
-- ---@param path string
-- ---@param width number
-- ---@param height number
-- ---@param alpha number
-- ---@param draw boolean Draws the Image
-- ---@param spreadpad boolean Adds padding when used with AutoSpread...()
-- ---@return table ImageData
-- function AutoImage(path, width, height, border, spreadpad)
-- 	local w, h = UiGetImageSize(path)
-- 	width = AutoDefault(width, (height == nil and UiWidth() or (height * (w / h))))
-- 	height = AutoDefault(height, width * (h / w))
-- 	border = AutoDefault(border, 0)
-- 	draw = AutoDefault(draw, true)
-- 	spreadpad = AutoDefault(spreadpad, true)

-- 	UiPush()
-- 	UiImageBox(path, width, height, border, border)
-- 	UiPop()

-- 	local hover = UiIsMouseInRect(width, height)

-- 	local data = { hover = hover, rect = { w = width, h = height } }
-- 	if draw then AutoHandleSpread(AutoGetSpread(), data, 'draw', spreadpad) end

-- 	return data
-- end

-- ---Creates a handy little marker, doesnt effect anything, purely visual
-- ---@param size number, Default is 1
-- function AutoMarker(size)
-- 	size = AutoDefault(size, 1) / 2
-- 	UiPush()
-- 	UiAlign('center middle')
-- 	UiScale(size, size)
-- 	UiColor(unpack(AutoSpecialColor))
-- 	UiImage('ui/common/dot.png')
-- 	UiPop()
-- end

--#endregion
--#region Cursed

---Loads a string as `return <lua_string>`
---@param lua_string string
---@return boolean success
---@return unknown? return_value
function AutoParse(lua_string)
	local formatted = 'return ' .. lua_string
	local success, func = pcall(loadstring, formatted)
	if success and func then
		local result_success, result = pcall(func)
		if result_success and result then
			return true, result
		end
	end
	
	return false
end

---A very sinful way to pipe raw code into the registry, use in combination with `AutoCMD_Parse`
---@param path string
---@param luastr string
function AutoCMD_Pipe(path, luastr)
	local keys = ListKeys(path)
	local newkey = AutoKey(path, #keys + 1)
	
	SetString(newkey, luastr)
end

---A very sinful way to parse raw code from the registry, use in combination with `AutoCMD_Pipe`
---
---_God is dead and we killed her._
---@param path string
---@return table<{ cmd:string, result:any }>
function AutoCMD_Parse(path)
	local results = {}
	for index = 1, #ListKeys(path) do
		local key = AutoKey(path, index)
		local cmd = GetString(key)
		local success, func = pcall(loadstring, cmd)
		
		if success and func then -- if it fails, then the cmd is discarded and we do not try to fix it
			results[#results + 1] = {
				cmd = cmd,
				result = func()
			}
		end
		
		ClearKey(key)
	end
	
	return results
end

function AutoCrash()
	Spawn '<vehicle/>'
end

--#endregion