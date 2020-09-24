app <- ShinyDriver$new("../../")
app$snapshotInit("mytest3")

app$setInputs(menu = "new")
app$snapshot()
