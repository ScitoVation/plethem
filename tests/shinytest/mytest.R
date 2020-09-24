app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$setInputs(menu = "load")
app$setInputs(files = "click")
app$setInputs(loadProjectFile = "click")
app$setInputs(files = "click")
app$setInputs(loadProjectFile = "click")
app$snapshot()
