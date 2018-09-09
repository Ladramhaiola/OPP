with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with System.Multiprocessors; use System.Multiprocessors;
with data;
procedure Lab1 is

   N : Integer := 2;

   package LabData is new data(N);
   package Float_IO is new Ada.Text_IO.Float_IO(Float);

   use Float_IO;
   use LabData;

   cpu0: CPU_Range := 0;
   cpu1: CPU_Range := 1;
   cpu2: CPU_Range := 2;

   F1Result: Vector;
   F2Result: Matrix;
   F3Result: Vector;

   procedure tasks is
      task T1 is
         pragma Priority(1);
         pragma Storage_Size(10000);
         pragma CPU(cpu0);
      end;

      task body T1 is
         vecA, vecB, vecC: Vector;
         mA, mE: Matrix;
      begin
         Put_Line("T1 Started...");
         randomVector(vecA);
         randomVector(vecB);
         randomVector(vecC);
         randomMatrix(mA);
         randomMatrix(mE);
         Func1(vecA, vecB, vecC, mA, mE, F1Result);
      end;

      task T2 is
         pragma Priority(2);
         pragma Storage_Size(10000);
         pragma CPU(cpu1);
      end;

      task body T2 is
         mG, mH, mK, mL: Matrix;
      begin
         Put_Line("T2 Started...");
         randomMatrix(mG);
         randomMatrix(mH);
         randomMatrix(mK);
         randomMatrix(mL);
         Func2(mG, mH, mK, mL, F2Result);
      end;

      task T3 is
         pragma Priority(3);
         pragma Storage_Size(10000);
         pragma CPU(cpu2);
      end;

      task body T3 is
         vecO: Vector := newVector("random");
         mO: Matrix := newMatrix("random");
         mS: Matrix := newMatrix("random");
         mT: Matrix := newMatrix("random");
      begin
         Put_Line("T3 Started...");
         Func3(vecO, mO, mS, mT, F3Result);
      end;

   begin
      null;
   end tasks;

begin
   tasks;
   Put_Line("");
   Put_Line("Task1 Result:");
   Put_Line("");
   print(F1Result);
   Put_Line("");
   Put_Line("Task2 Result:");
   Put_Line("");
   print(F2Result);
   Put_Line("Task3 Result:");
   Put_Line("");
   print(F3Result);
   Put_Line("");
end Lab1;
