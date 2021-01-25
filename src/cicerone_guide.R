guide <- Cicerone$
  new()$
  step(
    "guidetitle",
    "User Guide",
    "Please read the user guide in its entirety if you're a new user!"
  )$
  step(
    "checkbox",
    "First-time User Tools",
    "Click here to use the algorithm selector and data formatter tools to help you decide which algorithm to control FDR and how you should format your data."
  )$
  step(
    "upload",
    "Upload your file",
    "Once you upload your csv file of p-values, a dropdown menu of algorithms will then appear. Click one to navigate to that algorithm."
  )
