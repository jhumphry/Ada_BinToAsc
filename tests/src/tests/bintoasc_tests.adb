-- BinToAsc_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with BinToAsc_Suite;

with AUnit.Run;
with AUnit.Reporter.Text;

procedure BinToAsc_Tests is
   procedure Run is new AUnit.Run.Test_Runner (BinToAsc_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   AUnit.Reporter.Text.Set_Use_ANSI_Colors(Reporter, True);
   Run (Reporter);
end BinToAsc_Tests;
