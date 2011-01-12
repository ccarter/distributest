require 'rubygems'
require 'erlectricity'
require 'distributest/test_runner'

#Setting the DB_PREFIX env variable off of first arg passed in to the exec process.
ENV["DB_PREFIX"] = ARGV[0]

module Distributest
  def self.start
    receive do |f|
      f.when([:file, String]) do |text|
        pass_results, fail_results, profile, total_time_for_file = Distributest::TestRunner.new.run_rspec_file(text)
        pass_results = pass_results.to_s
        #Basically letting master know it ran a file even though it didn't have runnable specs
        if (pass_results.nil? || pass_results.length == 0) && (fail_results.nil? || fail_results.length == 0)
          f.send!([:pass_results, "."])
        else
          f.send!([:pass_results, pass_results]) unless pass_results.nil? or pass_results.length == 0
          f.send!([:fail_results, fail_results]) unless fail_results.nil? or fail_results.length == 0
          f.send!([:total_time_for_file, text, total_time_for_file])
          f.send!([:profile, text, profile])
        end
        f.send!(:ready_for_file)
        f.receive_loop
      end
      #stops the loop causing it to stop
      f.when(:stop) { f.send!([:port_shutdown, "normal"]) }

      f.when([:object, Any]) do |obj|
        puts "in ruby in Any with obj #{obj.inspect}"
        f.send!([:barf, "Barf in ruby with obj #{obj.inspect}"])
        f.receive_loop
      end
    end
  end
end