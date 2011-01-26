require 'distributest/formatter'
module Distributest
  class TestRunner
    
    def capture_output
      @stdout = StringIO.new
      @stderr = StringIO.new
      $stderr = @stderr
      $stdout = @stdout
    end
    
    def captured_output
      [@stdout.string, @stderr.string]
    end
    
    def run_rspec_file(file)
      capture_output
      
      start_time = Time.now
      begin
        require 'spec'

      rescue LoadError => ex
        return ex.to_s
      end

      output = []
      errors = []
      profile = []

      Spec::Runner.options.instance_variable_set(:@formatters,
        [Distributest::Formatter::BasicFormat.new(output, errors, profile)])
      Spec::Runner.options.instance_variable_set(:@example_groups, [])
      Spec::Runner.options.instance_variable_set(:@files, [file])
      Spec::Runner.options.instance_variable_set(:@files_loaded, false)

      if file == ""
        return nil
      else
        Spec::Runner.options.run_examples
      end
      end_time = Time.now
      total_time_for_file = end_time - start_time
      
      out, err = captured_output
      
      return output, errors, profile, total_time_for_file, out + err
    end
  end
end