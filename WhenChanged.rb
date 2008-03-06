class WhenChanged
  def initialize( *fnames )
    @last_time = nil
    @fnames = fnames
    puts "Watching #{@fnames.join(',')}"

  end

  def run( interval )
    while true
      sleep(interval)
      ctime = @fnames.map { |fn| File.stat(fn).ctime }.max
      if @last_time.nil? or ctime > @last_time
        puts(ctime - @last_time) unless @last_time.nil?
        puts `sh ./test.sh`
        @last_time = ctime
      end
    end
  end
end

WhenChanged.new( *ARGV ).run( 0.5 )
