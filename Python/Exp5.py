#Program to demonstrate Classes, Constructor and Objects
#defining a class
class Student:
    #defining a parameterized constructor
    def __init__(self, name, age):
        self.name=name
        self.age=age

    #defining a method
    def introduce(self):
        return (f"Hello, my name is {self.name}!\n I am {self.age} years old\n\n")

#creating an object
stud1=Student("Ayush", 20)

#calling the method
print(stud1.introduce())

class Plant:
    #non parameterized constructor
    def __init__(self):
        self.name=None
        self.type=None

    def describe(self):
        return f"Plant name= {self.name} \n Plant type= {self.type}\n"

plant1=Plant()

print(plant1.describe())
#assiging values to attributes
plant1.name="Apple"
plant1.type="Tree"

print(plant1.describe())
