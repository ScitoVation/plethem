app <- ShinyDriver$new("../../")
app$snapshotInit("mytest2")

app$setInputs(menu = "load")
app$setInputs(files = "click")
app$setInputs(loadProjectFile = "click")
app$setInputs(files = "click")
app$setInputs(loadProjectFile = "click")
app$snapshot()
