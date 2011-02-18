require File.dirname(__FILE__) + "/spec_helper.rb"

describe "it should fail" do
  it "should have an exception" do
    "blah".boom.should == true
  end
  it "should have assertion failure" do
    1.should == 2
  end
end