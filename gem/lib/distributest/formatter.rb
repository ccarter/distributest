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
      
      def example_pending(example_proxy, message, deprecated_pending_location = nil)
        output << '*'
      end

      def dump_failure(counter, failure)
      end

    end
  end
end