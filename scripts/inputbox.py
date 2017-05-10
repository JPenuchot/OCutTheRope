# Code of simple input box in python

from Tkinter import *

master = Tk()
master.title(sys.argv[1])
master.geometry("300x100")
e = Entry(master)
e.pack()

e.focus_set()

def callback():
	print e.get()
	quit()

b = Button(master, text = "OK", width = 10, command = callback)
b.pack()

mainloop()