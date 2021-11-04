class HelloWorld
  def show
    puts "Hello, world!"
  end
end

class Calculator
  def x
    5
  end

  def y
    8
  end

  def sum
    result = self.x + self.y

    if result > 10
      puts "Big number"
    else
      puts "Small number"
    end

    puts result
  end

  def dsum (x, y)
    puts x.abs + y
  end
end

program1 = HelloWorld.new
program1.show

program2 = Calculator.new
program2.sum
program2.dsum(-12, 8)
