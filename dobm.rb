#!/usr/bin/ruby

require "open3"
require "pp"

class Run 
  attr_reader :file, :real, :user, :weight
  def initialize( file, real, user, weight = 1 )
    @file = file
    @real, @user = real, user
    @weight = weight 
  end

  def avg( other )
    raise "tags didn't match" if other.file != @file
    total = @weight + other.weight
    new_real = (@real * @weight + other.real * other.weight) / total
    new_user = (@user * @weight + other.user * other.weight) / total
    return Run.new( @file, new_real, new_user, total )
  end

  def to_s
    "#{@file}: #{@real}, #{@user} (#{@weight})"
  end
end

def markit( cmd )
  lines = nil
  Open3.popen3("time ./hiccup #{cmd}") do |stdin, stdout, stderr| 
    lines = stderr.readlines
  end
  cleaned = lines.join.strip
  if cleaned =~ /^\s*([0-9.]+)\s+real\s*([0-9.]+)/
    return Run.new( cmd, $1.to_f, $2.to_f )
  else
    return cleaned
  end
end

def run( file, times = 2 )
  times.times { puts( markit( file ) ) }
end

BOTH = "both.tcl"
run( BOTH, 3 )

BENCH = "bench.tcl"
run( BENCH, 3 )
