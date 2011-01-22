require 'distributest/formatter'
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

      return output, errors, profile, total_time_for_file
    end
  end
end