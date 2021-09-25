	integer function ksext3(b1,b2,b3)
c return sign-extended value of 3-byte integer
c for LSB byte order machine
	integer*1 b1,b2,b3,block(4)
	integer*4 k /-1/
	equivalence (k,block)
	block(3)=b1
	block(2)=b2
	block(1)=b3
	ksext3 = k
	end

