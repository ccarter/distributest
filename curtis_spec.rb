#rspec_dir = '/Users/testadmin/blackbox/autohost_api/vendor/plugins/rspec/lib'
#$LOAD_PATH.unshift(rspec_dir)
require 'rubygems'
require 'erlectricity'

#Setting the DB_PREFIX env variable off of first arg passed in to the exec process.
#Didn't seem to work by just setting env variable in the first place
ENV["DB_PREFIX"] = ARGV[0]

module WideTest
  module Formatter
    
    require 'spec/runner/formatter/base_formatter'
    
    class BasicFormat < Spec::Runner::Formatter::BaseFormatter
      attr_accessor :output, :errors
      
      def initialize(output, errors)
        @output = output
        @errors = errors
      end
      
      def dump_summary(duration, example, failure, pending)
        #hiding summary atm
      end
      
      def example_passed(proxy)
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

module WideTest
  class TestRunner

    def run_rspec_file(file)
      begin
        require 'spec'
        
      rescue LoadError => ex
        return ex.to_s
      end

      output = []
      errors = []
      Spec::Runner.options.instance_variable_set(:@formatters, [
        WideTest::Formatter::BasicFormat.new(
          output, errors
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

      return output, errors
    end
  end
end

#currently just used to make passing specs ...
def mangle_output(output)
  output.to_s
end

receive do |f|
  f.when([:file, String]) do |text|
    pass_results, fail_results = WideTest::TestRunner.new.run_rspec_file(text)
    pass_results = mangle_output(pass_results)
    if (pass_results.nil? || pass_results.length == 0) && (fail_results.nil? || fail_results.length == 0)
      #f.send!([:no_results, text])
      f.send!([:pass_results, "."])
    else
      f.send!([:pass_results, pass_results]) unless pass_results.nil? or pass_results.length == 0
      f.send!([:fail_results, fail_results]) unless fail_results.nil? or fail_results.length == 0
    end
    f.receive_loop
  end
  f.when([:object, Any]) do |obj|
    puts "in ruby in Any with obj #{obj.inspect}"
    f.send!([:barf, "Barf in ruby with obj #{obj.inspect}"])
    f.receive_loop
  end
end