# 280_Degree_Audit

This projects purpose is to create an audit using Lisp, to determine whether or not a student is able to graduate based on their previous and planned courses.
This is all through a backwards chaining expert system.  

The requirements for graduation are:
- they have the desired number of hours for their major with passing grades (it will signify how many they need or how many extra credits they have)
- all required courses have been taken (or are in plan) and have passed all wih a grade above a D+
- courses can only count as a requirement or elective, not both

After deciding if the person can graduate, a response will be outputted if they pass or fail, as well as if they are above the credit hours or below, and what courses they need or could fulfilled requirements. 


## HOW TO RUN:
After defining your catalog, transcript, and degree requirements (examples below), then you can use the code by calling 

(grad-check catalog transcript grad-requirements)

Keep in mind that this is all in Lisp and will have to be ran accordingly.


## EXAMPLES:

catalog:
 ((COS102 3) (COS104 2) (COS109 3) (COS120 4) (COS121 4) (COS130 3) (COS143 3) (COS170 3)
 (COS232 3) (COS243 3) (COS265 3) (COS270 3) (COS280 3) (COS284 3)
 (COS310 1) (COS311 3) (COS321 3) (COS320 3) (COS323 3) (COS331 3) (COS333 3) (COS340 3) (COS343 3)
 (COS350 3) (COS351 3) (COS355 3) (COS360 3) (COS370 3) (COS380 3) (COS381 3) (COS382 3) (COS393 3) (COS394 3)
 (COS411 3) (COS421 3) (COS424 3) (COS425 3) (COS432 3) (COS433 3) (COS435 3) (COS436 3)
 (COS450 3) (COS492 3) (COS493 1)
 (MAT151 4) (MAT215 3) (MAT210 4) (MAT230 4) (MAT240 4) (MAT245 4) (MAT251 4) (MAT352 4)
 (SYS214 3) (SYS352 3) (SYS401 3) (SYS402 3) (SYS403 3) (SYS411 3) ))
 
 transcript:
 ((transcript 
 (COS102 A-) (COS109 A) (COS120 B) (COS121 B+) (COS143 A-) (COS243 B+) (COS265 B) (COS284 B-) (MAT151 A-) (MAT210 B))
 (plan COS492 COS493 COS280 COS331 COS340 COS350 SYS214 SYS411 MAT215)))
 
 degree requirements:
 ((major-hours 64)
 (required COS102 COS120 COS121 COS143 COS243 COS265 COS284 COS492 COS493 MAT151 MAT215
           (OR COS311 COS321)
           (OR COS320 COS382 COS435)
           (OR COS393 COS394 COS450)
           (OR MAT210 MAT352) )
 (electives COS104 COS109 COS130 COS170 
            COS232 COS270 COS280 
            COS310 COS323 COS331 COS333 COS340 COS343 COS350 
            COS351 COS355 COS360 COS370 COS380 COS381
            COS411 COS421 COS424 COS425 COS432 COS433 COS436
            MAT230 MAT240 MAT245 MAT251
            SYS214 SYS352 SYS401 SYS402 SYS403 SYS411) ))
