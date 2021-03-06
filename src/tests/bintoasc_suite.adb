-- BinToAsc_Suite
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with BinToAsc_Suite.Misc_Tests;
with BinToAsc_Suite.Base16_Tests;
with BinToAsc_Suite.Base32_Tests;
with BinToAsc_Suite.Base64_Tests;
with BinToAsc_Suite.Base85_Tests;

package body BinToAsc_Suite is
   use AUnit.Test_Suites;

   Result : aliased Test_Suite;

   Test_Misc : aliased Misc_Tests.Misc_Test;
   Test_Base16 : aliased Base16_Tests.Base16_Test;
   Test_Base32 : aliased Base32_Tests.Base32_Test;
   Test_Base64 : aliased Base64_Tests.Base64_Test;
   Test_Base85 : aliased Base85_Tests.Base85_Test;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Misc'Access);
      Add_Test (Result'Access, Test_Base16'Access);
      Add_Test (Result'Access, Test_Base32'Access);
      Add_Test (Result'Access, Test_Base64'Access);
      Add_Test (Result'Access, Test_Base85'Access);
      return Result'Access;
   end Suite;

end BinToAsc_Suite;
