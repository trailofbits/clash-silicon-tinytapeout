`timescale 100us/100us
`define default_netname none

// a small verification shim.

module tb( input  clk
         , input  rst
         , input [5:0] instr
         , output wire [4:0] io_out
         , output wire cjump
         );

    /* verilator lint_off STMTDLY */
    initial begin
        $dumpfile ("tb.vcd");
        $dumpvars (0, tb);
        #1;
    end
    /* verilator lint_on STMTDLY */

    top top_tb( .clk(clk)
              , .rst(rst)
              , .instr(instr)
              , .io_out(io_out)
              , .cjump(cjump)
              );

endmodule
