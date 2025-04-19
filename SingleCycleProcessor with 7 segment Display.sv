module top (input logic clk, reset,
 output logic[31:0] writedata, dataadr,
 output logic[31:0] pc, instr, readdata,
 output logic memwrite);
 mips mips (clk, reset, pc, instr, memwrite, dataadr, writedata, readdata);
 imem imem (pc[7:0], instr);
 dmem dmem (clk, memwrite, dataadr, writedata, readdata);
endmodule

module dmem (input logic clk, we,
 input logic[31:0] a, wd,
 output logic[31:0] rd);
 logic [31:0] RAM[63:0];

 assign rd = RAM[a[31:2]]; 
 always_ff @(posedge clk)
 if (we)
 RAM[a[31:2]] <= wd; 
endmodule

module imem (input logic [7:0] addr, output logic [31:0] instr);
always_comb
 case (addr) 
8'h00: instr = 32'h20020005; 
8'h04: instr = 32'h2003000c; 
8'h08: instr = 32'h2067fff7; 
8'h0c: instr = 32'h00e22025; 
8'h10: instr = 32'h00642824;
8'h14: instr = 32'h00a42820;
8'h18: instr = 32'h10a7000a;
8'h1c: instr = 32'h0064202a;
8'h20: instr = 32'h10800001;
8'h24: instr = 32'h20050000;
8'h28: instr = 32'h00e2202a;
8'h2c: instr = 32'h00853820;
8'h30: instr = 32'h00e23822;
8'h34: instr = 32'hac670044;
8'h38: instr = 32'h8c020050;
8'h3c: instr = 32'h08000011;
8'h40: instr = 32'h20020001;
8'h44: instr = 32'hac020054;
8'h48: instr = 32'h08000012; 
8'h50: instr = 32'h20040018; 
8'h54: instr = 32'h20050008; 
8'h58: instr = 32'h04110000; 
8'h5c: instr = 32'h20060003; 
8'h60: instr = 32'h20070006; 
8'h64: instr = 32'h03E00008; 
 default: instr = {32{1'bx}}; 
 endcase
endmodule

module mips (input logic clk, reset,
 output logic[31:0] pc,
 input logic[31:0] instr,
 output logic memwrite,
 output logic[31:0] aluout, writedata,
 input logic[31:0] readdata);
 logic memtoreg, pcsrc, zero, alusrc, regdst, regwrite, jump;
 logic [2:0] alucontrol;
 controller c (instr[31:26], instr[5:0], zero, memtoreg, memwrite, pcsrc,
 alusrc, regdst, regwrite, jump, alucontrol);

 datapath dp (clk, reset, memtoreg, pcsrc, alusrc, regdst, regwrite, jump,
 alucontrol, zero, pc, instr, aluout, writedata, readdata);
endmodule

module controller(input logic[5:0] op, funct,
 input logic zero,
 output logic memtoreg, memwrite,
 output logic pcsrc, alusrc,
 output logic regdst, regwrite,
 output logic jump,
 output logic[2:0] alucontrol);
 logic [1:0] aluop;
 logic branch;
 maindec md (op, memtoreg, memwrite, branch, alusrc, regdst, regwrite,
jump, aluop); 
 aludec ad (funct, aluop, alucontrol); 
 assign pcsrc = branch & zero; 
endmodule

module maindec (input logic[5:0] op,
 output logic memtoreg, memwrite, branch,
 output logic alusrc, regdst, regwrite, jump,
 output logic[1:0] aluop );
 logic [8:0] controls;
 assign {regwrite, regdst, alusrc, branch, memwrite,
 memtoreg, aluop, jump} = controls; 

 always_comb 
 case(op) 
 6'b000000: controls <= 9'b110000100; 
 6'b100011: controls <= 9'b101001000; 
 6'b101011: controls <= 9'b001010000; 
 6'b000100: controls <= 9'b000100010; 
 6'b001000: controls <= 9'b101000000; 
 6'b000010: controls <= 9'b000000001; 
 default: controls <= 9'bxxxxxxxxx; 
 endcase
endmodule

module aludec (input logic[5:0] funct,
 input logic[1:0] aluop,
 output logic[2:0] alucontrol);
 always_comb
 case(aluop)
 2'b00: alucontrol = 3'b010; 
 2'b01: alucontrol = 3'b110; 
 default: case(funct) 
 6'b100000: alucontrol = 3'b010; 
 6'b100010: alucontrol = 3'b110; 
 6'b100100: alucontrol = 3'b000; 
 6'b100101: alucontrol = 3'b001; 
 6'b101010: alucontrol = 3'b111; 
 default: alucontrol = 3'bxxx; 
 endcase
 endcase
endmodule

module datapath (input logic clk, reset, memtoreg, pcsrc, alusrc, regdst,
 input logic regwrite, jump,
 input logic[2:0] alucontrol,
 output logic zero,
 output logic[31:0] pc,
 input logic[31:0] instr,
 output logic[31:0] aluout, writedata,
 input logic[31:0] readdata);
 logic [4:0] writereg;
 logic [31:0] pcnext, pcnextbr, pcplus4, pcbranch;
 logic [31:0] signimm, signimmsh, srca, srcb, result;

 flopr #(32) pcreg(clk, reset, pcnext, pc);
 adder pcadd1(pc, 32'b100, pcplus4);
 sl2 immsh(signimm, signimmsh);
 adder pcadd2(pcplus4, signimmsh, pcbranch);
 mux2 #(32) pcbrmux(pcplus4, pcbranch, pcsrc,
 pcnextbr);
 mux2 #(32) pcmux(pcnextbr, {pcplus4[31:28],
 instr[25:0], 2'b00}, jump, pcnext);

 regfile rf (clk, regwrite, instr[25:21], instr[20:16], writereg,
 result, srca, writedata);
 mux2 #(5) wrmux (instr[20:16], instr[15:11], regdst, writereg);
 mux2 #(32) resmux (aluout, readdata, memtoreg, result);
 signext se (instr[15:0], signimm);

 mux2 #(32) srcbmux (writedata, signimm, alusrc, srcb);
 alu alu (srca, srcb, alucontrol, aluout, zero);
endmodule

module regfile (input logic clk, we3,
 input logic[4:0] ra1, ra2, wa3,
 input logic[31:0] wd3,
 output logic[31:0] rd1, rd2);
 logic [31:0] rf [31:0];

 always_ff@(posedge clk)
 if (we3)
 rf [wa3] <= wd3;
 assign rd1 = (ra1 != 0) ? rf [ra1] : 0;
 assign rd2 = (ra2 != 0) ? rf[ ra2] : 0;
endmodule

`timescale 1ns / 1ps

module alu(
    input  logic [31:0] a, b,
    input  logic [2:0]  alucont,
    output logic [31:0] result,
    output logic zero
);
    always_comb begin
        case (alucont)
            3'b010: result = a + b;         
            3'b110: result = a - b;          
            3'b000: result = a & b;           
            3'b001: result = a | b;           
            3'b111: result = (a < b) ? 32'b1 : 32'b0; // SLT
            3'b011: result = b >>> a[4:0];    
            3'b100: result = ~a;              
            default: result = 32'bx;          
        endcase
    end
    
    assign zero = (result == 32'b0) ? 1'b1 : 1'b0;

endmodule

module adder (input logic[31:0] a, b,
 output logic[31:0] y);
 assign y = a + b;
endmodule

module sl2 (input logic[31:0] a,
 output logic[31:0] y);
 assign y = {a[29:0], 2'b00}; 
endmodule

module signext (input logic[15:0] a,
 output logic[31:0] y);

 assign y = {{16{a[15]}}, a}; 
endmodule

module flopr #(parameter WIDTH = 8)
 (input logic clk, reset,
 input logic[WIDTH-1:0] d,
 output logic[WIDTH-1:0] q);
 always_ff@(posedge clk, posedge reset)
 if (reset) q <= 0;

 else q <= d;
endmodule
module mux2 #(parameter WIDTH = 8)
 (input logic[WIDTH-1:0] d0, d1,
 input logic s,
 output logic[WIDTH-1:0] y);

 assign y = s ? d1 : d0;
endmodule
