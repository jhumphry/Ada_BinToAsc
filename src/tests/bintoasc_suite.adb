-- BinToAsc_Suite
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with BinToAsc_Suite.Base16_Tests;

package body BinToAsc_Suite is
   use AUnit.Test_Suites;

   Result : aliased Test_Suite;

   Test_Base16 : aliased Base16_Tests.Base16_Test;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Base16'Access);
      return Result'Access;
   end Suite;

end BinToAsc_Suite;
