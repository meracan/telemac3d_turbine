


#check
gfortran -J./mod -I./mod ../source/check.f90 check.f90 -o ./out/check.out && ./out/check.out

#qsort
gfortran -J./mod -I./mod  ../source/a_qsort.f90 qsort.f90 -o ./out/qsort.out && ./out/qsort.out

#math
gfortran -J./mod -I./mod ../source/check.f90 ../source/math.f90  ../source/a_qsort.f90 math.f90 -o ./out/math.out && ./out/math.out

#mat4
gfortran -J./mod -I./mod ../source/check.f90 ../source/mat4.f90 mat4.f90 -o ./out/mat4.out && ./out/mat4.out



#qhull
gfortran -J./mod -I./mod  ../source/a_qsort.f90 ../source/file_csv.f90  ../source/math.f90 ../source/qhull.f90 qhull.f90 -o ./out/qhull.out && ./out/qhull.out

#turbine
gfortran -J./mod -I./mod  ../source/check.f90 ../source/file_csv.f90 ../source/mat4.f90 ../source/math.f90 ../source/a_qsort.f90 ../source/qhull.f90  ../source/turbine.f90 ../source/turbines.f90 turbine.f90 -o ./out/turbine.out && ./out/turbine.out

#geometry
gfortran ../source/geometry.f90 ../source/mat4.f90  ../source/file_csv.f90 geometry.f90 -o geometry.out && ./geometry.out