cwd = File.dirname(__FILE__)

def spec(n)
  <<-DOC
require File.dirname(__FILE__) + "/../spec_helper.rb"
5.times do
  describe "#{n} file spec" do
    it "should pass" do
      sleep(4)
      1.should == 1
    end
  end
end
DOC
end

5.times do |n|
  File.open(cwd + "/generated_specs/#{n}_spec.rb", 'w') do |f|
    spec(n)
    f.write(spec(n))
  end
end