# Load dependency management system to improve portability and
# reproducibility. This stores all the packages and dependancies 
# in the project folder, rather than my personal library.
# More details: http://rstudio.github.io/packrat/

# To reproduce the environment that this analysis was 
# conducted in (ie. you are on a computer that is not mine)

# Unbundle the packrat project, generating a 
# project directory with libraries restored. You should
# be right to run the rest of the code in this repository 
# after this:

packrat::unbundle()

# Setup instructions for me to use when preparing this repository

# just do this once when starting to word on project
# packrat::init()


# do this periodically to see if I've added any packages that haven't
# been locally stored. Want to do this each time I work on the project.
# packrat::status()

# if the status says I have some packages that aren't yet in the
# packrat. Can run every time I work on the project & add or remove a 
# package
# packrat::snapshot() 

# if the status says it found packages I'm not using, 
# use this to clean them out
# packrat::clean()

# if this project has been copyied onto a new machine, or if it's been
# updated via version control, then we need to run this:
# packrat::restore()

# Finally, when everything is in order, bundle a packrat project, for easy sharing.
# packrat::bundle()