### MOVE THIS TO YOUR RAILS APP/SPEC FOLDER
require 'rubygems'
require 'erlectricity'

#Setting the DB_PREFIX env variable off of first arg passed in to the exec process.
#Didn't seem to work by just setting env variable in the first place
ENV["DB_PREFIX"] = ARGV[0]

module Distributest
  module Formatter
    
    require 'spec/runner/formatter/base_formatter'
    
    class BasicFormat < Spec::Runner::Formatter::BaseFormatter
      attr_accessor :output, :errors
      
      def initialize(output, errors, profile)
        @output = output
        @errors = errors
        @profile = profile
      end
      
      def dump_summary(duration, example, failure, pending)
        #hiding summary atm
      end
      
      def example_started(example)
        @time = Time.now
      end
      
      def example_passed(example)
        @profile << [example.description, Time.now - @time]
        output << '.'
      end
      
      def example_failed(example_proxy, counter, failure)
        errors << 'F'
        errors << failure.header.to_s
        errors << failure.exception.to_s
        errors << failure.exception.backtrace.to_s
      end
      
      def dump_failure(counter, failure)
      end

    end
  end
end

module Distributest
  class TestRunner

    def run_rspec_file(file)
      start_time = Time.now
      begin
        require 'spec'
        
      rescue LoadError => ex
        return ex.to_s
      end

      output = []
      errors = []
      profile = []
      
      Spec::Runner.options.instance_variable_set(:@formatters, [
        Distributest::Formatter::BasicFormat.new(
          output, errors, profile
        )
      ])
      Spec::Runner.options.instance_variable_set(
      :@example_groups, []
      )
      Spec::Runner.options.instance_variable_set(
      :@files, [file]
      )
      Spec::Runner.options.instance_variable_set(
      :@files_loaded, false
      )
      
      if file == ""
        return nil
      else
        Spec::Runner.options.run_examples
      end
      end_time = Time.now
      total_time_for_file = end_time - start_time

      return output, errors, profile, total_time_for_file
    end
  end
end

#currently just used to make passing specs ...
def mangle_output(output)
  output.to_s
end

receive do |f|
  f.when([:file, String]) do |text|
    pass_results, fail_results, profile, total_time_for_file = Distributest::TestRunner.new.run_rspec_file(text)
    pass_results = mangle_output(pass_results)
    #Basically letting master know it ran a file even though it didn't have runnable specs
    if (pass_results.nil? || pass_results.length == 0) && (fail_results.nil? || fail_results.length == 0)
      f.send!([:pass_results, "."])
    else
      f.send!([:pass_results, pass_results]) unless pass_results.nil? or pass_results.length == 0
      f.send!([:fail_results, fail_results]) unless fail_results.nil? or fail_results.length == 0
      f.send!([:total_time_for_file, text, total_time_for_file])
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
