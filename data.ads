generic N: Integer;

package data is

   type Vector is array(1..N) of Float;
   type Matrix is array(1..N) of Vector;
   
   function "*"(Left, Right: Vector) return Float;
   function "+"(Left, Right: Vector) return Vector;
   function "+"(Left, Right: Matrix) return Matrix;
   function "*"(Left, Right: Matrix) return Matrix;
   function "*"(Left: Matrix; Right: Vector) return Vector;
   function sort(vec: Vector) return Vector;
   
   function newVector(nmod: String := "random") return Vector;
   function newMatrix(nmod: String := "random") return Matrix;
   
   procedure Func1(A, B, C: in Vector; MA, ME: in Matrix; vec: out Vector);
   procedure Func2(mG, mH, mK, mL: in Matrix; m: out Matrix);
   procedure Func3(O: in Vector; MO, MS, MT: in Matrix; vec: out Vector);

   procedure print(vec: in Vector);
   procedure print(mat: in Matrix);
   
   procedure randomVector(vec: out Vector);
   procedure randomMatrix(mat: out Matrix);
    
   procedure fillVector(vec: out Vector; by: in Float := 0.0);
   procedure fillMatrix(mat: out Matrix; by: in Float := 0.0);
   
end data;
