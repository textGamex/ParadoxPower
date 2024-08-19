﻿using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using ParadoxPower.CSharp;
using ParadoxPower.Parser;
using ParadoxPower.Process;
using ParadoxPower.Utilities;

namespace ParadoxPower.Benchmark;

[MemoryDiagnoser]
public class Program
{
    private const string Text = """
                                
                                state={
                                	id=607
                                	name="STATE_607"
                                	manpower=34289800
                                	
                                	state_category = town
                                	
                                	buildings_max_level_factor=1.000
                                
                                	history={
                                		owner = CHI
                                		add_core_of = CHI
                                		add_core_of = PRC
                                		buildings = {
                                			infrastructure = 3 #was: 5
                                			industrial_complex = 2
                                		}
                                		victory_points = {
                                			9958 1 
                                		}
                                
                                		1938.10.25 = {		
                                			buildings = {
                                				industrial_complex = 4
                                				infrastructure = 4 #was: 8
                                				air_base = 2
                                				arms_factory = 2
                                			}
                                			JAP = {
                                				set_province_controller = 1004
                                				set_province_controller = 1139
                                				set_province_controller = 4010
                                				set_province_controller = 4114
                                				set_province_controller = 4144
                                				set_province_controller = 4606
                                				set_province_controller = 6932
                                				set_province_controller = 7074
                                				set_province_controller = 7085
                                				set_province_controller = 7129
                                				set_province_controller = 9958
                                				set_province_controller = 9995
                                				set_province_controller = 10098
                                				set_province_controller = 11931
                                			} 			
                                		}
                                	}
                                
                                	provinces={
                                		1004 1139 1603 4010 4066 4114 4144 4519 4547 4606 6932 7074 7085 7126 7129 7459 7508 7540 7568 7656 9958 9995 10098 10364 10434 10446 11931 12012 12426 
                                	}
                                
                                	local_supplies=3.0 
                                }
                                """;
    
    private Node _node;
    
    [GlobalSetup]
    public void Setup()
    {
	    _node = Parsers.ProcessStatements("123", "123",
		    Parsers.ParseScriptFile("123.txt", Text).GetResult()).Child("state").Value;
    }
    
    public static void Main(string[] args)
    {
        BenchmarkRunner.Run<Program>();
    }
    
    [Benchmark]
    public void Raw()
    {
        _node.SetValue("local_supplies", Child.NewLeafC(new Leaf("local_supplies", Types.Value.NewFloat(3), Position.Range.Zero, Types.Operator.Equals)));
    }

    [Benchmark(Baseline = true)]
    public void Optimized()
    {
	    _node.SetTagOpt("local_supplies", Child.NewLeafC(new Leaf("local_supplies", Types.Value.NewFloat(3), Position.Range.Zero, Types.Operator.Equals)));
    }
}