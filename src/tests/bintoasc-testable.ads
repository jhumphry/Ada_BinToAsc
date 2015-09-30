-- BinToAsc.Testable
-- Exposure some private internals of BinToAsc for unit testing

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

generic
package BinToAsc.Testable is

   procedure Check_Make_Reverse_Alphabet (T : in out Test_Cases.Test_Case'Class);

end BinToAsc.Testable;
