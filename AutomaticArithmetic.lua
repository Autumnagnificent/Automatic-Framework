-- Logistic function, Can be used for juicy UI among other things. Graph attached to visualize
-- https://www.desmos.com/calculator/cmmwrjtyit?invertedColors
function logistic(v, max, steep, offset)
	return max / (1 + math.exp((v - offset) * steep))
end

-- Challenge by @TallTim and @1ssnl to make the smallest rounding function
-- 'n' is the number you want rounded, 'd' is the decimal, so something like 0.1, 0.01, 1, 2, 100, 2.5
function round(n,d)x=1/d return math.floor(n*x+.5)/x end

function clamp(val, min, max)
	if val < min then
		return min
	elseif val > max then
		return max
	else
		return val
	end
end

function big(a, b)
	if math.abs( a ) > math.abs( b ) then
		return a
	else
		return b
	end
end

function small(a, b)
	if math.abs( a ) < math.abs( b ) then
		return a
	else
		return b
	end
end

function count(t)
	local c = 0
	for i in pairs(t) do
		c = c + 1
	end

	return c
end

function rndVec(length, precision)
	precision = precision or 0.01
	local m = 1/precision
	local v = VecNormalize(Vec(math.random(-m,m), math.random(-m,m), math.random(-m,m)))
	return VecScale(v, length)	
end

function lerpUnclamped(a, b, t)
	return (1-t)*a + t*b
end

function move(a, b, t)
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

function vecDist(a, b)
	return math.sqrt( (a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2 )
end

function dist(a, b)
	return math.abs(a - b)
end