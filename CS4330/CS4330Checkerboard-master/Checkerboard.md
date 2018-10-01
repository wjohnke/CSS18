# 1. Reflection on challenge
Looking back on this assignment, I probably handled the constraints a bit lazily. I relied somewhat
heavily on concepts/exact code that we had covered in class the week during the project assignment, instead of
prior knowledge or any kind of creativity that still remained within the bounds of code structure and
readability. I believe I did well on the assignment, however the smoothness of the board upon resizing
could be greatly improved, especially when comparing to the original assignment. The main challenges
I struggled with were the resizing of the board, as well as the actual structure of the object construction
and where to separate all of my concerns within the controller vs. the FXML document vs. the model.


# 2. Understanding the programming concepts enough to complete the challenge
At the time, I think I could have understood the concepts a bit better. I was still pretty unfamiliar
with the changeListeners, and structuring my code under MVC still felt a little new. I would argue that
I understand those much better now, and understanding the importance and usefulness of both greatly
impacts my appreciation of them. I was also a bit out of practice using FXML within Java at the time,
so it was beneficial to get a refresh on that.

# 3. I believe I met most all of the requirements, with exception to having a "build" method on my
Checkerboard class. I'm not certain why I chose not to add this, but I moved the same functionality
of building an anchorpane off of existing data fields into the actual controller, which doesn't make
much sense in that spot. In terms of the other project specifications, such as the required methods,
abilities to change colors/sizes/number of rectangles, as well as the actual UI setup, I think I followed
those much more closely.

# 4. My solution matching posted solution
In terms of the setup of the actual Checkerboard object, and the change listeners on resizing and methods to
change the colors of the board, my solution was fairly similar to the posted solution. I could have
focused more on eliminating code reuse, although I felt I did a fairly good job of that with my use of
the "refresh" method, which was similar to the "renderBoard" method. However, these two methods differed
in that I did not use a "build" method on the actual Checkerboard object (even though I believe it was 
in the project specification), and didn't construct the anchorpane from a field within the object, based
off the data that was already held. My refresh function was clearly not centered around a separation
of concerns, and it didn't make sense to do the building of the anchorpane within the ui controller.

# 5. Improvements
Moving forward, I think I could improve on a bit of the structure of my application, as well as a bit
of the logic behind where I placed certain things and how I chose to handle resizing of the board. 
I still am trying to understand change listeners a bit more, and event handling/lambda functions, although
I'm much more clear on that now. 
