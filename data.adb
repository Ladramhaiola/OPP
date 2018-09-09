with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Generic_Array_Sort;

package body data is

   type Random_Value is range 1..20;
   
   package Rand_Int is new Ada.Numerics.Discrete_Random(Random_Value);
   package Float_IO is new Ada.Text_IO.Float_IO(Float);
   use Rand_Int;
   
   gen: Rand_Int.Generator;
   
   
   procedure print(vec: in Vector) is
   begin
      Ada.Text_IO.Put("(");
      for i in vec'Range loop
         Float_IO.Put(vec(i), Aft=>1, Exp=>0);
         if i /= vec'Last then
            Ada.Text_IO.Put(", ");
         end if;
      end loop;
      Ada.Text_IO.Put_Line(")");
   end print;
   
   procedure print(mat: in Matrix) is
   begin
      Ada.Text_IO.Put_Line("Matrix: ");
      for i in 1..N loop
         print(mat(i));
      end loop;
      Ada.Text_IO.Put_Line("");
   end print;
   
   procedure randomVector(vec: out Vector) is
   begin
      for i in 1..N loop
         Rand_Int.Reset(gen);
         vec(i) := FLOAT(Rand_Int.Random(gen));
      end loop;
   end randomVector;
   
   procedure randomMatrix(mat: out Matrix) is
   begin
      for i in 1..N loop
         randomVector(mat(i));
      end loop;
   end randomMatrix;
   
   procedure fillVector(vec: out Vector; by: in Float := 0.0) is
   begin
      for i in 1..N loop
         vec(i) := by;
      end loop;
   end fillVector;
   
   procedure fillMatrix(mat: out Matrix; by: in Float := 0.0) is
   begin
      for i in 1..N loop
         fillVector(mat(i));
      end loop;
   end fillMatrix;
   
   function newVector(nmod: String := "random") return Vector is
      vec: Vector;
   begin
      if nmod = "zeroes" then
         fillVector(vec);
      elsif nmod = "ones" then
         fillVector(vec, 1.0);
      elsif nmod = "random" then
         randomVector(vec);
      else
         Ada.Text_IO.Put_Line("Unknown mode -> setting default");
         fillVector(vec);
      end if;
      
      return vec;
   end newVector;
   
   function newMatrix(nmod: String := "random") return Matrix is
      mat: Matrix;
   begin
      for i in 1..N loop
         mat(i) := newVector(nmod);
      end loop;
      
      return mat;
   end newMatrix;
   
   function sort(vec: Vector) return Vector is
      Result: Vector;
      m: Float;
   begin
      -- copy data from in Vector to Result Vector
      for i in 1..N loop
         Result(i) := vec(i);
      end loop;
      
      -- simple bubble sort of Result Vector
      for i in reverse 1..N loop
         for j in 1..(i - 1) loop
            if Result(j) > Result(j + 1) then
               m := Result(j);
               Result(j) := Result(j + 1);
               Result(j + 1) := m;
            end if;
         end loop;
      end loop;
      
      return Result;
   end sort;
  
   function "*"(Left, Right: Vector) return Float is
      Result: Float := 0.0;
   begin
      if Left'Last /= Right'Last then
         raise Constraint_Error with "vectors of different size";
      end if;
      for i in Left'Range loop
         Result := Result + Left(i) * Right(i);
      end loop;
      
      return Result;
   end "*";
   
   function "*"(Left, Right: Matrix) return Matrix is
      Result: Matrix;
   begin
      fillMatrix(Result);
      for i in 1..N loop
         for j in 1..N loop
            for k in 1..N loop
               Result(i)(j) := Result(i)(j) + Left(i)(k)*Right(k)(j);
            end loop;
         end loop;
      end loop;
      
      return Result;
   end "*";
   
   function "+"(Left, Right: Vector) return Vector is
      resultV: Vector;
   begin
      for i in Left'Range loop
         resultV(i) := Left(i) + Right(i);
      end loop;
      
      return resultV;
   end "+";
  
   function "+"(Left, Right: Matrix) return Matrix is
      Result: Matrix;
   begin
      for i in 1..N loop
         Result(i) := Left(i) + Right(i);
      end loop;
      
      return Result;
   end "+";
   
   function "*"(Left: Matrix; Right: Vector) return Vector is
      Result: Vector;
   begin
      fillVector(Result);
      for i in 1..N loop
         for j in 1..N loop
            Result(i) := Result(i) + Left(i)(j) * Right(i);
         end loop;
      end loop;
      
      return Result;
   end "*";
   
   function "*"(Left: Vector; Right: Matrix) return Vector is
      begin
      return Right * Left;
   end "*";
   
   procedure Func1(A, B, C: in Vector; MA, ME: in Matrix; vec: out Vector) is
   begin
      vec := sort(A) + sort(B) + (MA * ME) * sort(C);
   end Func1;
   
   procedure Func2(mG, mH, mK, mL: in Matrix; m: out Matrix) is
   begin
      m := mG + (mH * mK) + mL;
   end Func2;
   
   procedure Func3(O: in Vector; MO, MS, MT: in Matrix; vec: out Vector) is
   begin
      vec := sort(O * MO) * (MS * MT);
   end Func3;
   
end data;
