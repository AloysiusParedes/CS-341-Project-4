// Divvy Ride Analysis
//
// << Aloysius Paredes >>
// U. Of Illinois, Chicago
// CS 341, Fall 2016
// HW #04

#light

// Prompt user for a folder name, and then return a list of all the 
// files in that folder.  Returns empty list if folder doesn't exist
// or cannot be found.
let GetFilenames() = 
  printf "Please enter folder name> "
  let folder = System.Console.ReadLine()
  if not (System.IO.Directory.Exists folder) then
    []
  else
    folder
    |> System.IO.Directory.GetFiles
    |> Array.toList
//end GetFilenames()

// Parse one line of CSV data from a Divvy data file.
// trip_id,bikeid,tripduration,from_station_id,to_station_id,gender,birthyear
// Right now the function returns a list of two integer elements: [tripID; bikeID].
let private ParseLine (line:string) = 
  let elements = line.Split(',')
  let tripID = System.Convert.ToInt32(elements.[0])
  let bikeID = System.Convert.ToInt32(elements.[1])
  let duration = System.Convert.ToInt32(elements.[2])
  let fromStationID = System.Convert.ToInt32(elements.[3])
  let toStationID = System.Convert.ToInt32(elements.[4])

  //change the gender to integer values
  //male = 1, female = 2, ? = 3
  let genderTranslate gender =
    match gender with
    | "Male"    -> 1
    | "Female"  -> 2
    | _         -> 3

  let gender = genderTranslate elements.[5] 
  let birthYear = System.Convert.ToInt32(elements.[6])

  //return the values as a small list of integers
  [tripID; bikeID; duration; fromStationID; toStationID; gender; birthYear] 
//end ParseLine(line:string)


// 
// Parses 1 file of Divvy data, where the format of each line is 
// discussed above; returns back a list of elements where the format
// of each element is discussed above.
// NOTE: the "|>" means pipe the data from one function to
// the next.  The code below is equivalent to letting a var
// hold the value and then using that var in the next line.
let private ParseDivvyFile filename = 
  System.IO.File.ReadLines(filename)
  |> Seq.skip 1  // skip header row:
  |> Seq.map ParseLine
  |> Seq.toList
//end ParseDivvyFile

//function to return the number of members
let countMembers data =
  //create a sublist from element 5 (gender = 1 or 2 or 3) from the data list
  let riders = List.map (fun sublist -> List.item 5 sublist ) data
  //create sublist of males
  let males = riders |> List.filter(fun x -> x = 1)
  //create sublist of females
  let females = riders |> List.filter(fun x -> x = 2)
  //sum up the number of males and number of females
  let count = males.Length + females.Length
  //return the sum
  count
//end countMembers

//function to create a sublist of number of members from the data list
let getMembers data = 
  let y = List.map(fun x -> countMembers x) data
  y

//function to return number of nonmembers
let countNonMembers data =
  //create a sublist from element 5 (gender = 1 or 2 or 3) from the data list
  let riders = List.map(fun sublist -> List.item 5 sublist) data
  //create a sublist of unknowns (not male and not female)
  let unknowns = riders |> List.filter(fun x -> x = 3) 
  //return the length of the unknowns list
  let count = unknowns.Length
  count
//end countNonMembers

//function to create a sublist of number of nonmembers from the data list
let getNonMembers data =
  let y = List.map(fun x -> countNonMembers x) data
  y

//function to return men percentage
let calculateMenPercentage data =
  //create a sublist from element 5 (gender = 1 or 2 or 3) from the data list
  let riders = List.map(fun sublist -> List.item 5 sublist) data
  //create sublist of males
  let males = riders |> List.filter(fun x -> x = 1)
  //create sublist of females
  let females = riders |> List.filter(fun x -> x = 2)
  //sum up the number of males and number of females
  let total = float(males.Length + females.Length)
  //calculate percentage = numMales / total
  let percentage = (float(males.Length) / total) * 100.0
  //return the percentage of men
  percentage
//end calculateMenPercentage

//function to create a sublist of % of men
let getMenPercentage data =
  let y = List.map(fun x -> calculateMenPercentage x) data
  y

//function to return women percentage
let calculateWomenPercentage data =
  //create a sublist from element 5 (gender = 1 or 2 or 3) from the data list
  let riders = List.map(fun sublist -> List.item 5 sublist) data
  //create sublist of males
  let males = riders |> List.filter(fun x -> x = 1)
  //create sublist of females
  let females = riders |> List.filter(fun x -> x = 2)
  //sum up the number of males and number of females
  let total = float(males.Length + females.Length)
  //calculate percentage = numMales / total
  let percentage = (float(females.Length) / total) * 100.0
  //return the percentage of women
  percentage
//end calculateMenPercentage

//function to create a sublist of % of women
let getWomenPercentage data =
  let y = List.map(fun x -> calculateWomenPercentage x) data
  y

//function to return number of males
let totalMale data = 
  //create a sublist from element 5 (gender = 1 or 2 or 3) from the data list
  let riders = List.map(fun sublist -> List.item 5 sublist) data
  //create sublist of males
  let males = riders |> List.filter(fun x -> x = 1)
  //return number of males
  males.Length
//end totalMale

//function to create a sublist of total number of males
let getMales data =
  let y = List.map(fun x -> totalMale x) data
  y

//function to return number of females
let totalFemale data = 
  //create a sublist from element 5 (gender = 1 or 2 or 3) from the data list
  let riders = List.map(fun sublist -> List.item 5 sublist) data
  //create sublist of males
  let females = riders |> List.filter(fun x -> x = 2)
  //return number of males
  females.Length
//end totalFemale

//function to create a sublist of total number of females
let getFemales data =
  let y = List.map(fun x -> totalFemale x) data
  y


//function to calculate male age
let calculateMaleAge data = 
  //variable to hold the current year
  let curYear = System.DateTime.Now.Year
  //create a sublist from element 5 from the data list
  let riders = List.map(fun sublist -> List.item 5 sublist) data
  //create a sublist from element 6 from the data list
  let allBirthYears = List.map(fun sublist -> List.item 6 sublist) data
  //create a sublist of tuples holding gender and birthyear
  let riderAndBirths = List.zip riders allBirthYears
  //filter out the females and unknowns
  let maleBirths = riderAndBirths |> List.filter(fun x -> fst x = 1)
  //create a list of just birthyears (should be male)
  let males, birthYears = List.unzip maleBirths
  //create a sublist of ages
  let ages = List.map(fun x -> if x = 0 then 0 else curYear - x) birthYears
  //return the sum of all ages per month
  List.sum ages
//end calculateMaleAge

//function to create a sublist of male ages
let getMaleAges data =
  let y = List.map(fun x -> calculateMaleAge x) data
  y

//function to calculate male age
let calculateFemaleAge data = 
  //variable to hold the current year
  let curYear = System.DateTime.Now.Year
  //create a sublist from element 5 from the data list
  let riders = List.map(fun sublist -> List.item 5 sublist) data
  //create a sublist from element 6 from the data list
  let allBirthYears = List.map(fun sublist -> List.item 6 sublist) data
  //create a sublist of tuples holding gender and birthyear
  let riderAndBirths = List.zip riders allBirthYears
  //filter out the males and unknowns
  let femaleBirths = riderAndBirths |> List.filter(fun x -> fst x = 2)
  //create a list of just birthyears (should be female)
  let females, birthYears = List.unzip femaleBirths
  //create a sublist of ages
  let ages = List.map(fun x -> if x = 0 then 0 else curYear - x) birthYears
  //return the sum of all ages per month
  List.sum ages
//end calculateFemaleAge

//function to create a sublist of male ages
let getFemaleAges data =
  let y = List.map(fun x -> calculateFemaleAge x) data
  y

//calculate durations 1-30min
let calculateDuration1 data =
  //create a sublist from element 2 from the data list
  let durations = List.map(fun sublist -> List.item 2 sublist) data
  //filter out anything that's not 1-30min
  let neededDuration = durations |> List.filter(fun x -> x <= 1800)
  //return the number of 1-30min durations
  neededDuration.Length
//function to create a sublist of 1-30 min durations
let getDurations1 data =
  let y = List.map(fun x -> calculateDuration1 x) data
  y

//calculate durations 31-60min
let calculateDuration2 data =
  //create a sublist from element 2 from the data list
  let durations = List.map(fun sublist -> List.item 2 sublist) data
  //filter out anything that's not 31-60min
  let neededDuration = durations |> List.filter(fun x -> x > 1800 && x <= 3600)
  //return the number of 31-60min durations
  neededDuration.Length
//function to create a sublist of 1-30 min durations
let getDurations2 data =
  let y = List.map(fun x -> calculateDuration2 x) data
  y

//calculate durations 61-120min
let calculateDuration3 data =
  //create a sublist from element 2 from the data list
  let durations = List.map(fun sublist -> List.item 2 sublist) data
  //filter out anything that's not 31-60min
  let neededDuration = durations |> List.filter(fun x -> x > 3600 && x <= 7200)
  //return the number of 61-120min durations
  neededDuration.Length
//function to create a sublist of 1-30 min durations
let getDurations3 data =
  let y = List.map(fun x -> calculateDuration3 x) data
  y

//calculate durations 121+min
let calculateDuration4 data =
  //create a sublist from element 2 from the data list
  let durations = List.map(fun sublist -> List.item 2 sublist) data
  //filter out anything that's not 121+min
  let neededDuration = durations |> List.filter(fun x -> x > 7200)
  //return the number of 121+min durations
  neededDuration.Length
//function to create a sublist of 1-30 min durations
let getDurations4 data =
  let y = List.map(fun x -> calculateDuration4 x) data
  y

[<EntryPoint>]
let main argv = 
  printfn ""
  printfn "** Divvy Ride Analysis **"
  printfn ""
  //
  let files = GetFilenames()
  //
  // input the data into a LIST OF LISTS, one per file.  The format
  // of each sub-list is determined by the "ParseDivvyFile" function:
  //
  let data = List.map ParseDivvyFile files
  //
  // printfn "%A" data   // debugging to see data format:
  //
  printfn ""
  printfn "** # of rides:  %A" (List.map List.length data)

  let numMembersList = getMembers data
  //get the members list
  printfn "** Members:     %A" (numMembersList)
  let numNonMembersList = getNonMembers data
  //get the nonmembers list
  printfn "** Non-Members: %A" (numNonMembersList)

  //get the percentage of men list
  let menPercentage = getMenPercentage data
  printfn ""
  printfn "** %% of men:    %A" (menPercentage)
  //get the percentage of women list
  let womenPercentage = getWomenPercentage data
  printfn "** %% of women:  %A" (womenPercentage)

  //variable to hold total number of males and females
  let totalNumMales = List.sum (getMales data)
  let totalNumFemales = List.sum (getFemales data)
  //variable to hold sum of male and female ages
  let maleAges = List.sum (getMaleAges data)
  let femaleAges = List.sum (getFemaleAges data)
  printfn ""
  printfn "** Average age:"
  //get average age of men
  printfn "   Men:   %A" (float(maleAges) / float(totalNumMales))
  //get average age of women
  printfn "   Women: %A" (float(femaleAges) / float(totalNumFemales))

  //variable to hold total rides
  let totalRides = float(List.sum (List.map List.length data))
  //variables to hold filtered durations
  let durations1 = getDurations1 data
  let durations2 = getDurations2 data
  let durations3 = getDurations3 data
  let durations4 = getDurations4 data

  printfn ""
  printfn "** Ride Durations:"
  printfn "   1..30   mins: %A (%A%%)" (List.sum durations1) ((float(List.sum durations1) / totalRides) * 100.0)
  printfn "   31..60  mins: %A (%A%%)" (List.sum durations2) ((float(List.sum durations2) / totalRides) * 100.0)
  printfn "   61..120 mins: %A (%A%%)" (List.sum durations3) ((float(List.sum durations3) / totalRides) * 100.0)
  printfn "   121+    mins: %A (%A%%)" (List.sum durations4) ((float(List.sum durations4) / totalRides) * 100.0)

  //
  // done:
  //
  printfn ""
  printfn "** Done **"
  printfn ""
  printfn ""
  0