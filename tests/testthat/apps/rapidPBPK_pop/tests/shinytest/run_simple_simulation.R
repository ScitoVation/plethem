app <- ShinyDriver$new("../../")
app$snapshotInit("run_simple_simulation")

app$setInputs(menu = "setup")
app$setInputs(modelSetupTabs = "Simulations")
app$setInputs(btn_run_sim = "click")
app$snapshot()
