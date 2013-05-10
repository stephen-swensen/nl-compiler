xl = list[int32]() in
x = 1 in 
y = 2 in 
while y < 2000 {
	xl.add(y);
 	temp = y in 
	y <- x + y;
	x <- temp 
}; xl