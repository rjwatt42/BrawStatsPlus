setNotation<-function(useLabels) {
  useLabels<-list(psig="psig",UD="D",P="0")
  
  switch(useLabels$psig,
         "psig"={pSigLabel<<-bquote(bold(p[.('sig')]))},
         "w"={pSigLabel<<-bquote(bold(w))}
         )

  LabelUD<<-useLabels$UD

  posChar<<-"+"
  nullChar<<-"0"
  switch(useLabels$P,
         "+"={
           pPlus<<-TRUE
         },
         "0"={
           pPlus<<-FALSE
         },
         "-"={
           pPlus<<-FALSE
           nullChar<<-'\u2013'
         }
  )
  

if (pPlus) {
  Ptypechar<-posChar 
} else {
  Ptypechar<-nullChar
}
Ltypechar<-posChar

pPlusLabel<<-paste0("P(",Ptypechar,")")

switch (LabelUD, 
        "U"={
          Plabel<<-bquote(bold(.(Pchar)^.(Ptypechar)))
          Llabel<<-bquote(bold(.(Lchar)^.(Ltypechar)))
          
          nonNullPositive<<-bquote(.(Zchar)^.(posChar)~'+sig')  # "Z+ +ve"
          nonNullNS<<-bquote(.(Zchar)^.(posChar) ~"ns")  # "Z+ -ve"
          nonNullNegative<<-bquote(.(Zchar)^.(posChar) ~"-sig")  # "Z+ -ve"
          nullPositive<<-bquote(.(Zchar)^.(nullChar) ~"+sig")   # "Z0 +ve"
          nullNS<<-bquote(.(Zchar)^.(nullChar) ~"ns")  # "Z0 -ve"
          nullNegative<<-bquote(.(Zchar)^.(nullChar) ~"-sig")  # "Z0 -ve"
        },
        "D"={
          Plabel<<-bquote(bold(.(Pchar)[.(Ptypechar)]))
          Llabel<<-bquote(bold(.(Lchar)[.(Ltypechar)]))
          
          nonNullPositive<<-bquote(.(Zchar)[.(posChar)] ~"+sig")  # "Z+ +ve"
          nonNullNS<<-bquote(.(Zchar)[.(posChar)] ~"ns")  # "Z+ -ve"
          nonNullNegative<<-bquote(.(Zchar)[.(posChar)] ~"-sig")  # "Z+ -ve"
          nullPositive<<-bquote(.(Zchar)[.(nullChar)] ~"+sig")   # "Z0 +ve"
          nullNS<<-bquote(.(Zchar)[.(nullChar)] ~"ns")  # "Z0 -ve"
          nullNegative<<-bquote(.(Zchar)[.(nullChar)] ~"-sig")  # "Z0 -ve"
        }
)
allPositive<<-bquote(.(Zchar) ~"+ve")
allNegative<<-bquote(.(Zchar) ~"ns")
}
