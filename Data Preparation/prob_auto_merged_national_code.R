library(readxl)
library(readr)
library(dplyr)
library(stringr)

#This code was produced by Dr. Lendie Follett with the assistance of ChatGPT to match up the original dataset with the national dataset to achieve appropriate weighting for total employment. 


# -------------------------------------------------------------------
# 1. READ BOTH FILES
# -------------------------------------------------------------------

national = read.csv(file.choose(), header=T) #your national data (excel). File name is "national_M2024_dl.xlsx". Need to convert this to CSV for use with this code.

#look at first few rows
head(national)

prob_auto = read.csv(file.choose(), header=T) #your P(auto) data (csv). File name is "Probability of Automation Data Set.csv"


#look at first few rows
head(prob_auto)


#Our goal is to add a column to national that has the same values as the "occupation"
#column in the prob_auto data

# -------------------------------------------------------------------
# 2. KEEP ONLY THE COLUMNS YOU NEED
# -------------------------------------------------------------------

national <- national %>%
  select( OCC_TITLE, O_GROUP, TOT_EMP) %>%  #keeping o_group_for now...  so we can only keep cases where o_group is "broad" (you can obviously change this)
  filter(O_GROUP == "detailed") %>%  #so we're not double summing with the major/minor/broad/detailed repetitions of each occ...
  select(-O_GROUP)

#see:
head(national)


# --------------------------------------------------
# 3. CLEANING FUNCTION
# --------------------------------------------------

clean_occ <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

national <- national %>%
  mutate(occ_clean = clean_occ(OCC_TITLE))

prob_auto <- prob_auto %>%
  mutate(occ_clean = clean_occ(occupation))

# --------------------------------------------------
# 4. BUILD LOOKUP FROM PROBABILITY DATA
# --------------------------------------------------

prob_levels <- prob_auto %>%
  distinct(occupation, occ_clean)

prob_titles_exact <- prob_auto$occupation
prob_titles_clean <- prob_auto$occ_clean

# --------------------------------------------------
# 5. CREATE OCC_TITLE_NEW
# --------------------------------------------------
# Order matters:
#   (a) exact raw-text match
#   (b) exact cleaned-text match
#   (c) manual case_when only for true mismatches
# --------------------------------------------------

national <- national %>%
  mutate(
    OCC_TITLE_NEW = case_when(
      OCC_TITLE %in% c(
        "Barbers",
        "Batch food makers",
        "Biological scientists",
        "Bookbinders",
        "Broadcast equipment operators",
        "Bus drivers",
        "Butchers and meat cutters",
        "Buyers, wholesale and retail trade",
        "Chemists",
        "Computer and peripheral equipment operators",
        "Computer software developers",
        "Computer systems analysts and computer scientists",
        "Concrete and cement workers",
        "Cooks",
        "Crossing guards",
        "Dancers",
        "Data entry keyers",
        "Dental Assistants",
        "Dental hygienists",
        "Dentists",
        "Designers",
        "Dispatchers",
        "Door-to-door sales, street sales, and news vendors",
        "Drafters",
        "Dressmakers, seamstresses, and tailors",
        "Drillers of earth",
        "Drillers of oil wells",
        "Drilling and boring machine operators",
        "Drywall installers",
        "Editors and reporters",
        "Electric power installers and repairers",
        "Electrical engineers",
        "Engineering technicians",
        "Engravers",
        "Excavating and loading machine operators",
        "Extruding and forming machine operators",
        "Food roasting and baking machine operators",
        "Foresters and conservation scientists",
        "Funeral directors",
        "Garage and service station related occupations",
        "Gardeners and groundskeepers",
        "General office clerks",
        "Geologists",
        "Grinding, abrading, buffing, and polishing workers",
        "Guards and police, except public service",
        "Hairdressers and cosmetologists",
        "Hand molders and shapers, except jewelers",
        "Health technologists and technicians, n.e.c.",
        "Heat treating equipment operators",
        "Helpers, constructions",
        "Housekeepers, maids, butlers, and cleaners",
        "Industrial engineers",
        "Industrial machinery repairers",
        "Insulation workers",
        "Insurance adjusters, examiners, and investigators",
        "Insurance underwriters",
        "Janitors",
        "Kindergarten and earlier school teachers",
        "Knitters, loopers, and toppers textile operatives",
        "Laborers, freight, stock, and material handlers, n.e.c.",
        "Lathe, milling, and turning machine operatives",
        "Lawyers and judges",
        "Locksmiths and safe repairers",
        "Locomotive operators: engineers and firemen",
        "Machine feeders and offbearers",
        "Machinery maintenance occupations",
        "Mail and paper handlers",
        "Mail carriers for postal service",
        "Mail clerks, outside of post office",
        "Managers in education and related fields",
        "Masons, tilers, and carpet installers",
        "Medical scientists",
        "Messengers",
        "Millwrights",
        "Misc. construction and related occupations",
        "Mixing and blending machine operators",
        "Molders and casting machine operators",
        "Musicians and composers",
        "Nail, tacking, shaping and joining mach ops (wood)",
        "Occupational therapists",
        "Operating engineers of construction equipment",
        "Other plant and system operators",
        "Packers and packagers by hand",
        "Painters, construction and maintenance",
        "Painters, sculptors, craft-artists, and print-makers",
        "Painting and decoration occupations",
        "Paper folding machine operators",
        "Paperhangers",
        "Patternmakers and model makers",
        "Paving, surfacing, and tamping equipment operators",
        "Physical scientists, n.e.c.",
        "Physical therapists",
        "Physicians",
        "Physicists and astronomists",
        "Plumbers, pipe fitters, and steamfitters",
        "Postal clerks, exluding mail carriers",
        "Power plant operators",
        "Precision makers, repairers, and smiths",
        "Primary school teachers",
        "Production helpers",
        "Programmers of numerically controlled machine tools",
        "Protective service, n.e.c.",
        "Psychologists",
        "Public transportation attendants and inspectors",
        "Purchasing agents and buyers of farm products",
        "Radiologic technologists and technicians",
        "Real estate sales occupations",
        "Recreation and fitness workers",
        "Recreation facility attendants",
        "Repairers of electrical equipment, n.e.c.",
        "Repairers of industrial electrical equipment",
        "Repairers of mechanical controls and valves",
        "Respiratory therapists",
        "Rollers, roll hands, and finishers of metal",
        "Sales demonstrators, promoters, and models",
        "Sales supervisors and proprietors",
        "Sawing machine operators and sawyers",
        "Secondary school teachers",
        "Secretaries and stenographers",
        "Separating, filtering, and clarifying machine operators",
        "Sheriffs, bailiffs, correctional institution officers",
        "Shoemaking machine operators",
        "Special education teachers",
        "Speech therapists",
        "Stock and inventory clerks",
        "Superv. of landscaping, lawn service, groundskeeping",
        "Supervisors of cleaning and building service",
        "Surveyors, cartographers, mapping scientists/techs",
        "Taxi cab drivers and chauffeurs",
        "Teacher's aides",
        "Teachers, n.e.c.",
        "Technical writers",
        "Telecom and line installers and repairers",
        "Textile cutting and dyeing machine operators",
        "Truck, delivery, and tractor drivers",
        "Typists",
        "Upholsterers",
        "Vehicle washers and equipment cleaners",
        "Vocational and educational counselors",
        "Washing, cleaning, and pickling machine operators",
        "Welders, solderers, and metal cutters",
        "Welfare service workers",
        "Winding and twisting textile and apparel operatives",
        "Writers and authors"
      ) ~ OCC_TITLE,
      # -----------------------------
      # MANAGEMENT
      # -----------------------------
      OCC_TITLE %in% c(
        "Chief Executives",
        "General and Operations Managers",
        "Legislators"
      ) ~ "Chief executives, public administrators, and legislators",
      
      OCC_TITLE %in% c(
        "Financial Managers"
      ) ~ "Financial managers",
      
      OCC_TITLE %in% c(
        "Compensation and Benefits Managers",
        "Human Resources Managers",
        "Training and Development Managers"
      ) ~ "Human resources and labour relations managers",
      
      OCC_TITLE %in% c(
        "Advertising and Promotions Managers",
        "Marketing Managers",
        "Public Relations Managers",
        "Fundraising Managers"
      ) ~ "Managers and specialists in marketing, advert., PR",
      
      OCC_TITLE %in% c(
        "Education Administrators, Kindergarten through Secondary",
        "Education Administrators, Postsecondary",
        "Education Administrators, All Other"
      ) ~ "Managers in education and related fields",
      
      OCC_TITLE %in% c(
        "Medical and Health Services Managers"
      ) ~ "Managers of medicine and health occupations",
      
      OCC_TITLE %in% c(
        "Property, Real Estate, and Community Association Managers"
      ) ~ "Managers of properties and real estate",
      
      OCC_TITLE %in% c(
        "Funeral Home Managers"
      ) ~ "Funeral directors",
      
      OCC_TITLE %in% c(
        "Administrative Services Managers",
        "Facilities Managers",
        "Emergency Management Directors",
        "Postmasters and Mail Superintendents",
        "Social and Community Service Managers",
        "Managers, All Other"
      ) ~ "Managers and administrators, n.e.c.",
      
      # -----------------------------
      # BUSINESS / FINANCE
      # -----------------------------
      OCC_TITLE %in% c(
        "Accountants and Auditors"
      ) ~ "Accountants and auditors",
      
      OCC_TITLE %in% c(
        "Insurance Underwriters"
      ) ~ "Insurance underwriters",
      
      OCC_TITLE %in% c(
        "Personal Financial Advisors",
        "Credit Analysts",
        "Financial Analysts",
        "Loan Officers",
        "Tax Examiners and Collectors, and Revenue Agents",
        "Tax Preparers",
        "Financial Specialists, All Other"
      ) ~ "Other financial specialists",
      
      OCC_TITLE %in% c(
        "Management Analysts"
      ) ~ "Management analysts",
      
      OCC_TITLE %in% c(
        "Human Resources Specialists",
        "Training and Development Specialists",
        "Labor Relations Specialists"
      ) ~ "Personnel, HR, training, and labour rel. specialists",
      
      OCC_TITLE %in% c(
        "Purchasing Agents, Except Wholesale, Retail, and Farm Products"
      ) ~ "Purchasing agents and buyers of farm products",
      
      OCC_TITLE %in% c(
        "Wholesale and Retail Buyers, Except Farm Products"
      ) ~ "Buyers, wholesale and retail trade",
      
      OCC_TITLE %in% c(
        "Purchasing Managers"
      ) ~ "Purchasing managers, agents, and buyers, n.e.c.",
      
      OCC_TITLE %in% c(
        "Agents and Business Managers of Artists, Performers, and Athletes",
        "Business Operations Specialists, All Other"
      ) ~ "Business and promotion agents",
      
      # -----------------------------
      # INSPECTION / ENGINEERING / SCIENCE
      # -----------------------------
      OCC_TITLE %in% c(
        "Construction and Building Inspectors"
      ) ~ "Construction inspectors",
      
      OCC_TITLE %in% c(
        "Compliance Officers"
      ) ~ "Inspectors and compliance officers, outside",
      
      OCC_TITLE %in% c(
        "Architects, Except Landscape and Naval",
        "Landscape Architects",
        "Naval Architects"
      ) ~ "Architects",
      
      OCC_TITLE %in% c(
        "Aerospace Engineers"
      ) ~ "Aerospace engineers",
      
      OCC_TITLE %in% c(
        "Materials Engineers",
        "Metallurgical Engineers"
      ) ~ "Metallurgical and materials engineers",
      
      OCC_TITLE %in% c(
        "Petroleum Engineers",
        "Mining and Geological Engineers, Including Mining Safety Engineers"
      ) ~ "Petroleum, mining, and geological engineers",
      
      OCC_TITLE %in% c(
        "Chemical Engineers"
      ) ~ "Chemical engineers",
      
      OCC_TITLE %in% c(
        "Civil Engineers"
      ) ~ "Civil engineers",
      
      OCC_TITLE %in% c(
        "Electrical Engineers"
      ) ~ "Electrical engineers",
      
      OCC_TITLE %in% c(
        "Industrial Engineers"
      ) ~ "Industrial engineers",
      
      OCC_TITLE %in% c(
        "Mechanical Engineers"
      ) ~ "Mechanical engineers",
      
      OCC_TITLE %in% c(
        "Bioengineers and Biomedical Engineers",
        "Environmental Engineers",
        "Health and Safety Engineers, Except Mining Safety Engineers and Inspectors",
        "Marine Engineers and Naval Architects",
        "Engineers, All Other"
      ) ~ "Engineers and other professionals, n.e.c.",
      
      OCC_TITLE %in% c(
        "Computer Systems Analysts",
        "Computer Scientists",
        "Information Security Analysts",
        "Computer Occupations, All Other"
      ) ~ "Computer systems analysts and computer scientists",
      
      OCC_TITLE %in% c(
        "Operations Research Analysts",
        "Logisticians"
      ) ~ "Operations and systems researchers and analysts",
      
      OCC_TITLE %in% c(
        "Actuaries"
      ) ~ "Actuaries",
      
      OCC_TITLE %in% c(
        "Mathematicians",
        "Statisticians",
        "Data Scientists"
      ) ~ "Mathematicians and statisticians",
      
      OCC_TITLE %in% c(
        "Physicists",
        "Astronomers"
      ) ~ "Physicists and astronomists",
      
      OCC_TITLE %in% c(
        "Chemists"
      ) ~ "Chemists",
      
      OCC_TITLE %in% c(
        "Atmospheric and Space Scientists"
      ) ~ "Atmospheric and space scientists",
      
      OCC_TITLE %in% c(
        "Geoscientists, Except Hydrologists and Geographers",
        "Geographers"
      ) ~ "Geologists",
      
      OCC_TITLE %in% c(
        "Hydrologists",
        "Physical Scientists, All Other"
      ) ~ "Physical scientists, n.e.c.",
      
      OCC_TITLE %in% c(
        "Agricultural and Food Scientists"
      ) ~ "Agricultural and food scientists",
      
      OCC_TITLE %in% c(
        "Biochemists and Biophysicists",
        "Microbiologists",
        "Zoologists and Wildlife Biologists",
        "Biological Scientists, All Other"
      ) ~ "Biological scientists",
      
      OCC_TITLE %in% c(
        "Conservation Scientists",
        "Foresters"
      ) ~ "Foresters and conservation scientists",
      
      OCC_TITLE %in% c(
        "Medical Scientists, Except Epidemiologists",
        "Epidemiologists"
      ) ~ "Medical scientists",
      
      # -----------------------------
      # HEALTH PROFESSIONALS
      # -----------------------------
      OCC_TITLE %in% c(
        "Physicians, All Other",
        "Family Medicine Physicians",
        "General Internal Medicine Physicians",
        "Pediatricians, General",
        "Surgeons, All Other",
        "Psychiatrists",
        "Emergency Medicine Physicians",
        "Obstetricians and Gynecologists",
        "Anesthesiologists",
        "Dermatologists",
        "Neurologists",
        "Ophthalmologists, Except Pediatric",
        "Pathologists",
        "Radiologists",
        "Physicians, Pathologists",
        "Physicians, Ophthalmologists"
      ) ~ "Physicians",
      
      OCC_TITLE %in% c(
        "Dentists, General",
        "Orthodontists",
        "Oral and Maxillofacial Surgeons",
        "Prosthodontists",
        "Dentists, All Other"
      ) ~ "Dentists",
      
      OCC_TITLE %in% c(
        "Veterinarians"
      ) ~ "Veterinarians",
      
      OCC_TITLE %in% c(
        "Optometrists"
      ) ~ "Optometrists",
      
      OCC_TITLE %in% c(
        "Podiatrists"
      ) ~ "Podiatrists",
      
      OCC_TITLE %in% c(
        "Registered Nurses"
      ) ~ "Registered nurses",
      
      OCC_TITLE %in% c(
        "Pharmacists"
      ) ~ "Pharmacists",
      
      OCC_TITLE %in% c(
        "Dietitians and Nutritionists"
      ) ~ "Dieticians and nutritionists",
      
      OCC_TITLE %in% c(
        "Respiratory Therapists"
      ) ~ "Respiratory therapists",
      
      OCC_TITLE %in% c(
        "Occupational Therapists"
      ) ~ "Occupational therapists",
      
      OCC_TITLE %in% c(
        "Physical Therapists"
      ) ~ "Physical therapists",
      
      OCC_TITLE %in% c(
        "Speech-Language Pathologists"
      ) ~ "Speech therapists",
      
      OCC_TITLE %in% c(
        "Physician Assistants"
      ) ~ "Physicians' assistants",
      
      # -----------------------------
      # EDUCATION / SOCIAL SCIENCE / LEGAL / ARTS
      # -----------------------------
      OCC_TITLE %in% c(
        "Preschool Teachers, Except Special Education",
        "Kindergarten Teachers, Except Special Education"
      ) ~ "Kindergarten and earlier school teachers",
      
      OCC_TITLE %in% c(
        "Elementary School Teachers, Except Special Education"
      ) ~ "Primary school teachers",
      
      OCC_TITLE %in% c(
        "Middle School Teachers, Except Special and Career/Technical Education",
        "Secondary School Teachers, Except Special and Career/Technical Education"
      ) ~ "Secondary school teachers",
      
      OCC_TITLE %in% c(
        "Special Education Teachers, Preschool",
        "Special Education Teachers, Kindergarten and Elementary School",
        "Special Education Teachers, Middle School",
        "Special Education Teachers, Secondary School",
        "Special Education Teachers, All Other"
      ) ~ "Special education teachers",
      
      OCC_TITLE %in% c(
        "Career/Technical Education Teachers, Secondary School",
        "Postsecondary Teachers",
        "Teachers and Instructors, All Other"
      ) ~ "Teachers, n.e.c.",
      
      OCC_TITLE %in% c(
        "Educational, Guidance, and Career Counselors and Advisors",
        "School Psychologists"
      ) ~ "Vocational and educational counselors",
      
      OCC_TITLE %in% c(
        "Librarians and Media Collections Specialists"
      ) ~ "Librarians",
      
      OCC_TITLE %in% c(
        "Archivists",
        "Curators",
        "Museum Technicians and Conservators"
      ) ~ "Archivists and curators",
      
      OCC_TITLE %in% c(
        "Economists",
        "Market Research Analysts and Marketing Specialists",
        "Survey Researchers"
      ) ~ "Economists, market and survey researchers",
      
      OCC_TITLE %in% c(
        "Psychologists, All Other",
        "Clinical and Counseling Psychologists",
        "Industrial-Organizational Psychologists"
      ) ~ "Psychologists",
      
      OCC_TITLE %in% c(
        "Sociologists",
        "Political Scientists",
        "Anthropologists and Archeologists",
        "Geographers",
        "Social Scientists and Related Workers, All Other"
      ) ~ "Social scientists and sociologists, n.e.c.",
      
      OCC_TITLE %in% c(
        "Urban and Regional Planners"
      ) ~ "Urban and regional planners",
      
      OCC_TITLE %in% c(
        "Social Workers",
        "Child, Family, and School Social Workers",
        "Healthcare Social Workers",
        "Mental Health and Substance Abuse Social Workers"
      ) ~ "Social workers",
      
      OCC_TITLE %in% c(
        "Clergy",
        "Directors, Religious Activities and Education",
        "Religious Workers, All Other"
      ) ~ "Clergy and religious workers",
      
      OCC_TITLE %in% c(
        "Social and Human Service Assistants"
      ) ~ "Welfare service workers",
      
      OCC_TITLE %in% c(
        "Lawyers",
        "Administrative Law Judges, Adjudicators, and Hearing Officers",
        "Judges, Magistrate Judges, and Magistrates"
      ) ~ "Lawyers and judges",
      
      OCC_TITLE %in% c(
        "Writers and Authors"
      ) ~ "Writers and authors",
      
      OCC_TITLE %in% c(
        "Technical Writers"
      ) ~ "Technical writers",
      
      OCC_TITLE %in% c(
        "Graphic Designers",
        "Industrial Designers",
        "Interior Designers",
        "Floral Designers",
        "Merchandise Displayers and Window Trimmers",
        "Designers, All Other"
      ) ~ "Designers",
      
      OCC_TITLE %in% c(
        "Music Directors and Composers",
        "Musicians and Singers"
      ) ~ "Musicians and composers",
      
      OCC_TITLE %in% c(
        "Actors",
        "Producers and Directors"
      ) ~ "Actors, directors, and producers",
      
      OCC_TITLE %in% c(
        "Craft Artists",
        "Fine Artists, Including Painters, Sculptors, and Illustrators",
        "Printmakers"
      ) ~ "Painters, sculptors, craft-artists, and print-makers",
      
      OCC_TITLE %in% c(
        "Photographers"
      ) ~ "Photographers",
      
      OCC_TITLE %in% c(
        "Dancers",
        "Choreographers"
      ) ~ "Dancers",
      
      OCC_TITLE %in% c(
        "Editors",
        "News Analysts, Reporters, and Journalists"
      ) ~ "Editors and reporters",
      
      OCC_TITLE %in% c(
        "Broadcast Announcers and Radio Disc Jockeys"
      ) ~ "Announcers",
      
      OCC_TITLE %in% c(
        "Athletes and Sports Competitors",
        "Coaches and Scouts",
        "Umpires, Referees, and Other Sports Officials",
        "Exercise Trainers and Group Fitness Instructors"
      ) ~ "Athletes, sports instructors, and officials",
      
      # -----------------------------
      # HEALTH TECHS / SCI TECHS / SOFTWARE / PARALEGAL
      # -----------------------------
      OCC_TITLE %in% c(
        "Clinical Laboratory Technologists and Technicians"
      ) ~ "Clinical laboratory technologies and technicians",
      
      OCC_TITLE %in% c(
        "Dental Hygienists"
      ) ~ "Dental hygienists",
      
      OCC_TITLE %in% c(
        "Medical Records Specialists",
        "Health Information Technologists and Medical Registrars"
      ) ~ "Health record technologists and technicians",
      
      OCC_TITLE %in% c(
        "Radiologic Technologists and Technicians"
      ) ~ "Radiologic technologists and technicians",
      
      OCC_TITLE %in% c(
        "Licensed Practical and Licensed Vocational Nurses"
      ) ~ "Licensed practical nurses",
      
      OCC_TITLE %in% c(
        "Cardiovascular Technologists and Technicians",
        "Diagnostic Medical Sonographers",
        "Magnetic Resonance Imaging Technologists",
        "Nuclear Medicine Technologists",
        "Ophthalmic Medical Technicians",
        "Surgical Assistants",
        "Surgical Technologists",
        "Veterinary Technologists and Technicians",
        "Health Technologists and Technicians, All Other"
      ) ~ "Health technologists and technicians, n.e.c.",
      
      OCC_TITLE %in% c(
        "Civil Engineering Technologists and Technicians",
        "Electrical and Electronic Engineering Technologists and Technicians",
        "Industrial Engineering Technologists and Technicians",
        "Mechanical Engineering Technologists and Technicians",
        "Engineering Technologists and Technicians, Except Drafters, All Other"
      ) ~ "Engineering technicians",
      
      OCC_TITLE %in% c(
        "Architectural and Civil Drafters",
        "Electrical and Electronics Drafters",
        "Mechanical Drafters",
        "Drafters, All Other"
      ) ~ "Drafters",
      
      OCC_TITLE %in% c(
        "Surveyors",
        "Cartographers and Photogrammetrists"
      ) ~ "Surveyors, cartographers, mapping scientists/techs",
      
      OCC_TITLE %in% c(
        "Biological Technicians"
      ) ~ "Biological technicians",
      
      OCC_TITLE %in% c(
        "Chemical Technicians"
      ) ~ "Chemical technicians",
      
      OCC_TITLE %in% c(
        "Forensic Science Technicians",
        "Forest and Conservation Technicians",
        "Geological and Hydrologic Technicians",
        "Nuclear Technicians",
        "Science Technicians, All Other"
      ) ~ "Other science technicians",
      
      OCC_TITLE %in% c(
        "Airline Pilots, Copilots, and Flight Engineers"
      ) ~ "Airplane pilots and navigators",
      
      OCC_TITLE %in% c(
        "Air Traffic Controllers"
      ) ~ "Air traffic controllers",
      
      OCC_TITLE %in% c(
        "Broadcast Technicians",
        "Sound Engineering Technicians"
      ) ~ "Broadcast equipment operators",
      
      OCC_TITLE %in% c(
        "Software Developers",
        "Software Quality Assurance Analysts and Testers",
        "Web Developers",
        "Web and Digital Interface Designers"
      ) ~ "Computer software developers",
      
      OCC_TITLE %in% c(
        "Computer Numerically Controlled Tool Programmers"
      ) ~ "Programmers of numerically controlled machine tools",
      
      OCC_TITLE %in% c(
        "Paralegals and Legal Assistants"
      ) ~ "Legal assistants and paralegals",
      
      # -----------------------------
      # SALES
      # -----------------------------
      OCC_TITLE %in% c(
        "First-Line Supervisors of Retail Sales Workers",
        "First-Line Supervisors of Non-Retail Sales Workers"
      ) ~ "Sales supervisors and proprietors",
      
      OCC_TITLE %in% c(
        "Insurance Sales Agents"
      ) ~ "Insurance sales occupations",
      
      OCC_TITLE %in% c(
        "Real Estate Brokers",
        "Real Estate Sales Agents"
      ) ~ "Real estate sales occupations",
      
      OCC_TITLE %in% c(
        "Securities, Commodities, and Financial Services Sales Agents"
      ) ~ "Financial service sales occupations",
      
      OCC_TITLE %in% c(
        "Advertising Sales Agents"
      ) ~ "Advertising and related sales jobs",
      
      OCC_TITLE %in% c(
        "Sales Engineers"
      ) ~ "Sales engineers",
      
      OCC_TITLE %in% c(
        "Retail Salespersons",
        "Parts Salespersons"
      ) ~ "Retail salespersons and sales clerks",
      
      OCC_TITLE %in% c(
        "Cashiers"
      ) ~ "Cashiers",
      
      OCC_TITLE %in% c(
        "Door-to-Door Sales Workers, News and Street Vendors, and Related Workers"
      ) ~ "Door-to-door sales, street sales, and news vendors",
      
      OCC_TITLE %in% c(
        "Demonstrators and Product Promoters",
        "Models"
      ) ~ "Sales demonstrators, promoters, and models",
      
      # -----------------------------
      # OFFICE / ADMIN SUPPORT
      # -----------------------------
      OCC_TITLE %in% c(
        "First-Line Supervisors of Office and Administrative Support Workers"
      ) ~ "Office supervisors",
      
      OCC_TITLE %in% c(
        "Computer Operators"
      ) ~ "Computer and peripheral equipment operators",
      
      OCC_TITLE %in% c(
        "Secretaries and Administrative Assistants, Except Legal, Medical, and Executive",
        "Executive Secretaries and Executive Administrative Assistants"
      ) ~ "Secretaries and stenographers",
      
      OCC_TITLE %in% c(
        "Typists and Word Processors"
      ) ~ "Typists",
      
      OCC_TITLE %in% c(
        "Interviewers, Except Eligibility and Loan",
        "Survey Interviewers and Enumerators"
      ) ~ "Interviewers, enumerators, and surveyors",
      
      OCC_TITLE %in% c(
        "Hotel, Motel, and Resort Desk Clerks"
      ) ~ "Hotel clerks",
      
      OCC_TITLE %in% c(
        "Reservation and Transportation Ticket Agents and Travel Clerks"
      ) ~ "Transportation ticket and reservation agents",
      
      OCC_TITLE %in% c(
        "Receptionists and Information Clerks"
      ) ~ "Receptionists and other information clerks",
      
      OCC_TITLE %in% c(
        "Order Clerks"
      ) ~ "Correspondence and order clerks",
      
      OCC_TITLE %in% c(
        "Human Resources Assistants, Except Payroll and Timekeeping"
      ) ~ "Human resources clerks, excl payroll and timekeeping",
      
      OCC_TITLE %in% c(
        "Library Technicians",
        "Library Assistants, Clerical"
      ) ~ "Library assistants",
      
      OCC_TITLE %in% c(
        "File Clerks"
      ) ~ "File clerks",
      
      OCC_TITLE %in% c(
        "Bookkeeping, Accounting, and Auditing Clerks"
      ) ~ "Bookkeepers and accounting and auditing clerks",
      
      OCC_TITLE %in% c(
        "Payroll and Timekeeping Clerks"
      ) ~ "Payroll and timekeeping clerks",
      
      OCC_TITLE %in% c(
        "Billing and Posting Clerks"
      ) ~ "Billing clerks and related financial records processing",
      
      OCC_TITLE %in% c(
        "Office and Administrative Support Workers, All Other"
      ) ~ "Mail and paper handlers",
      
      OCC_TITLE %in% c(
        "Office Machine Operators, Except Computer"
      ) ~ "Office machine operators, n.e.c.",
      
      OCC_TITLE %in% c(
        "Switchboard Operators, Including Answering Service"
      ) ~ "Telephone operators",
      
      OCC_TITLE %in% c(
        "Postal Service Clerks"
      ) ~ "Postal clerks, exluding mail carriers",
      
      OCC_TITLE %in% c(
        "Postal Service Mail Carriers"
      ) ~ "Mail carriers for postal service",
      
      OCC_TITLE %in% c(
        "Postal Service Mail Sorters, Processors, and Processing Machine Operators"
      ) ~ "Mail clerks, outside of post office",
      
      OCC_TITLE %in% c(
        "Messengers and Couriers"
      ) ~ "Messengers",
      
      OCC_TITLE %in% c(
        "Dispatchers, Except Police, Fire, and Ambulance"
      ) ~ "Dispatchers",
      
      OCC_TITLE %in% c(
        "Shipping, Receiving, and Inventory Clerks"
      ) ~ "Shipping and receiving clerks",
      
      OCC_TITLE %in% c(
        "Stockers and Order Fillers"
      ) ~ "Stock and inventory clerks",
      
      OCC_TITLE %in% c(
        "Meter Readers, Utilities"
      ) ~ "Meter readers",
      
      OCC_TITLE %in% c(
        "Weighers, Measurers, Checkers, and Samplers, Recordkeeping"
      ) ~ "Weighers, measurers, and checkers",
      
      OCC_TITLE %in% c(
        "Production, Planning, and Expediting Clerks"
      ) ~ "Material recording, sched., prod., plan., expediting cl.",
      
      OCC_TITLE %in% c(
        "Claims Adjusters, Examiners, and Investigators"
      ) ~ "Insurance adjusters, examiners, and investigators",
      
      OCC_TITLE %in% c(
        "Customer Service Representatives"
      ) ~ "Customer service reps, invest., adjusters, excl. insur.",
      
      OCC_TITLE %in% c(
        "Eligibility Interviewers, Government Programs"
      ) ~ "Eligibility clerks for government prog., social welfare",
      
      OCC_TITLE %in% c(
        "Bill and Account Collectors"
      ) ~ "Bill and account collectors",
      
      OCC_TITLE %in% c(
        "General Office Clerks"
      ) ~ "General office clerks",
      
      OCC_TITLE %in% c(
        "Tellers"
      ) ~ "Bank tellers",
      
      OCC_TITLE %in% c(
        "Proofreaders and Copy Markers"
      ) ~ "Proofreaders",
      
      OCC_TITLE %in% c(
        "Data Entry Keyers"
      ) ~ "Data entry keyers",
      
      OCC_TITLE %in% c(
        "Statistical Assistants"
      ) ~ "Statistical clerks",
      
      OCC_TITLE %in% c(
        "Teaching Assistants, Except Postsecondary"
      ) ~ "Teacher's aides",
      
      # -----------------------------
      # SERVICE / PROTECTIVE / FOOD / PERSONAL CARE
      # -----------------------------
      OCC_TITLE %in% c(
        "Maids and Housekeeping Cleaners"
      ) ~ "Housekeepers, maids, butlers, and cleaners",
      
      OCC_TITLE %in% c(
        "Laundry and Dry-Cleaning Workers"
      ) ~ "Laundry and dry cleaning workers",
      
      OCC_TITLE %in% c(
        "Firefighters",
        "Forest Fire Inspectors and Prevention Specialists"
      ) ~ "Fire fighting, fire prevention, and fire inspection occs",
      
      OCC_TITLE %in% c(
        "Police and Sheriff's Patrol Officers",
        "Detectives and Criminal Investigators"
      ) ~ "Police and detectives, public service",
      
      OCC_TITLE %in% c(
        "Correctional Officers and Jailers",
        "Bailiffs",
        "Sheriffs and Deputy Sheriffs"
      ) ~ "Sheriffs, bailiffs, correctional institution officers",
      
      OCC_TITLE %in% c(
        "Crossing Guards and Flaggers"
      ) ~ "Crossing guards",
      
      OCC_TITLE %in% c(
        "Security Guards",
        "Gambling Surveillance Officers and Gambling Investigators"
      ) ~ "Guards and police, except public service",
      
      OCC_TITLE %in% c(
        "Lifeguards, Ski Patrol, and Other Recreational Protective Service Workers",
        "Protective Service Workers, All Other"
      ) ~ "Protective service, n.e.c.",
      
      OCC_TITLE %in% c(
        "Bartenders"
      ) ~ "Bartenders",
      
      OCC_TITLE %in% c(
        "Waiters and Waitresses"
      ) ~ "Waiters and waitresses",
      
      OCC_TITLE %in% c(
        "Cooks, Fast Food",
        "Cooks, Institution and Cafeteria",
        "Cooks, Private Household",
        "Cooks, Restaurant",
        "Cooks, Short Order",
        "Cooks, All Other"
      ) ~ "Cooks",
      
      OCC_TITLE %in% c(
        "Food Preparation Workers"
      ) ~ "Food preparation workers",
      
      OCC_TITLE %in% c(
        "Dining Room and Cafeteria Attendants and Bartender Helpers",
        "Dishwashers",
        "Food Servers, Nonrestaurant",
        "Counter Attendants, Cafeteria, Food Concession, and Coffee Shop"
      ) ~ "Miscellanious food preparation and service workers",
      
      OCC_TITLE %in% c(
        "Dental Assistants"
      ) ~ "Dental Assistants",
      
      OCC_TITLE %in% c(
        "Nursing Assistants",
        "Orderlies",
        "Psychiatric Aides",
        "Home Health and Personal Care Aides"
      ) ~ "Health and nursing aides",
      
      OCC_TITLE %in% c(
        "First-Line Supervisors of Housekeeping and Janitorial Workers"
      ) ~ "Supervisors of cleaning and building service",
      
      OCC_TITLE %in% c(
        "First-Line Supervisors of Landscaping, Lawn Service, and Groundskeeping Workers"
      ) ~ "Superv. of landscaping, lawn service, groundskeeping",
      
      OCC_TITLE %in% c(
        "Landscaping and Groundskeeping Workers"
      ) ~ "Gardeners and groundskeepers",
      
      OCC_TITLE %in% c(
        "Janitors and Cleaners, Except Maids and Housekeeping Cleaners"
      ) ~ "Janitors",
      
      OCC_TITLE %in% c(
        "Pest Control Workers"
      ) ~ "Pest control occupations",
      
      OCC_TITLE %in% c(
        "Barbers"
      ) ~ "Barbers",
      
      OCC_TITLE %in% c(
        "Hairdressers, Hairstylists, and Cosmetologists"
      ) ~ "Hairdressers and cosmetologists",
      
      OCC_TITLE %in% c(
        "Amusement and Recreation Attendants"
      ) ~ "Recreation facility attendants",
      
      OCC_TITLE %in% c(
        "Tour and Travel Guides"
      ) ~ "Guides",
      
      OCC_TITLE %in% c(
        "Ushers, Lobby Attendants, and Ticket Takers"
      ) ~ "Ushers",
      
      OCC_TITLE %in% c(
        "Baggage Porters and Bellhops",
        "Concierges"
      ) ~ "Baggage porters, bellhops and concierges",
      
      OCC_TITLE %in% c(
        "Exercise Trainers and Group Fitness Instructors",
        "Recreation Workers"
      ) ~ "Recreation and fitness workers",
      
      OCC_TITLE %in% c(
        "Motion Picture Projectionists"
      ) ~ "Motion picture projectionists",
      
      OCC_TITLE %in% c(
        "Childcare Workers"
      ) ~ "Child care workers",
      
      OCC_TITLE %in% c(
        "First-Line Supervisors of Personal Service Workers"
      ) ~ "Supervisors of personal service jobs, n.e.c",
      
      OCC_TITLE %in% c(
        "Transportation Attendants, Except Flight Attendants"
      ) ~ "Public transportation attendants and inspectors",
      
      OCC_TITLE %in% c(
        "Animal Caretakers"
      ) ~ "Animal caretakers, except farm",
      
      # -----------------------------
      # REPAIR / CONSTRUCTION / EXTRACTION
      # -----------------------------
      OCC_TITLE %in% c(
        "First-Line Supervisors of Mechanics, Installers, and Repairers"
      ) ~ "Supervisors of mechanics and repairers",
      
      OCC_TITLE %in% c(
        "Automotive Service Technicians and Mechanics"
      ) ~ "Automobile mechanics and repairers",
      
      OCC_TITLE %in% c(
        "Bus and Truck Mechanics and Diesel Engine Specialists",
        "Mobile Heavy Equipment Mechanics, Except Engines"
      ) ~ "Bus, truck, and stationary engine mechanics",
      
      OCC_TITLE %in% c(
        "Aircraft Mechanics and Service Technicians"
      ) ~ "Aircraft mechanics",
      
      OCC_TITLE %in% c(
        "Small Engine Mechanics"
      ) ~ "Small engine repairers",
      
      OCC_TITLE %in% c(
        "Automotive Body and Related Repairers"
      ) ~ "Auto body repairers",
      
      OCC_TITLE %in% c(
        "Farm Equipment Mechanics and Service Technicians",
        "Heavy Vehicle and Mobile Equipment Service Technicians and Mechanics"
      ) ~ "Heavy equipement and farm equipment mechanics",
      
      OCC_TITLE %in% c(
        "Industrial Machinery Mechanics"
      ) ~ "Industrial machinery repairers",
      
      OCC_TITLE %in% c(
        "Maintenance Workers, Machinery"
      ) ~ "Machinery maintenance occupations",
      
      OCC_TITLE %in% c(
        "Electrical Power-Line Installers and Repairers",
        "Electrical and Electronics Repairers, Powerhouse, Substation, and Relay"
      ) ~ "Repairers of industrial electrical equipment",
      
      OCC_TITLE %in% c(
        "Computer, Automated Teller, and Office Machine Repairers"
      ) ~ "Repairers of data processing equipment",
      
      OCC_TITLE %in% c(
        "Home Appliance Repairers"
      ) ~ "Repairers of household appliances and power tools",
      
      OCC_TITLE %in% c(
        "Telecommunications Equipment Installers and Repairers, Except Line Installers"
      ) ~ "Telecom and line installers and repairers",
      
      OCC_TITLE %in% c(
        "Electrical and Electronics Repairers, Commercial and Industrial Equipment",
        "Electrical and Electronics Installers and Repairers, Transportation Equipment"
      ) ~ "Repairers of electrical equipment, n.e.c.",
      
      OCC_TITLE %in% c(
        "Heating, Air Conditioning, and Refrigeration Mechanics and Installers"
      ) ~ "Heating, air conditioning, and refrigeration mechanics",
      
      OCC_TITLE %in% c(
        "Camera and Photographic Equipment Repairers",
        "Medical Equipment Repairers",
        "Musical Instrument Repairers and Tuners",
        "Watch and Clock Repairers"
      ) ~ "Precision makers, repairers, and smiths",
      
      OCC_TITLE %in% c(
        "Locksmiths and Safe Repairers"
      ) ~ "Locksmiths and safe repairers",
      
      OCC_TITLE %in% c(
        "Control and Valve Installers and Repairers, Except Mechanical Door"
      ) ~ "Repairers of mechanical controls and valves",
      
      OCC_TITLE %in% c(
        "Elevator and Escalator Installers and Repairers"
      ) ~ "Elevator installers and repairers",
      
      OCC_TITLE %in% c(
        "Millwrights"
      ) ~ "Millwrights",
      
      OCC_TITLE %in% c(
        "Maintenance and Repair Workers, General"
      ) ~ "Mechanics and repairers, n.e.c.",
      
      OCC_TITLE %in% c(
        "First-Line Supervisors of Construction Trades and Extraction Workers"
      ) ~ "Supervisors of construction work",
      
      OCC_TITLE %in% c(
        "Tile and Stone Setters",
        "Carpet Installers",
        "Floor Sanders and Finishers",
        "Masons"
      ) ~ "Masons, tilers, and carpet installers",
      
      OCC_TITLE %in% c(
        "Carpenters"
      ) ~ "Carpenters",
      
      OCC_TITLE %in% c(
        "Drywall and Ceiling Tile Installers"
      ) ~ "Drywall installers",
      
      OCC_TITLE %in% c(
        "Electricians"
      ) ~ "Electricians",
      
      OCC_TITLE %in% c(
        "Electrical Power-Line Installers and Repairers"
      ) ~ "Electric power installers and repairers",
      
      OCC_TITLE %in% c(
        "Painters, Construction and Maintenance"
      ) ~ "Painters, construction and maintenance",
      
      OCC_TITLE %in% c(
        "Paperhangers"
      ) ~ "Paperhangers",
      
      OCC_TITLE %in% c(
        "Plasterers and Stucco Masons"
      ) ~ "Plasterers",
      
      OCC_TITLE %in% c(
        "Plumbers, Pipefitters, and Steamfitters"
      ) ~ "Plumbers, pipe fitters, and steamfitters",
      
      OCC_TITLE %in% c(
        "Cement Masons and Concrete Finishers",
        "Terrazzo Workers and Finishers"
      ) ~ "Concrete and cement workers",
      
      OCC_TITLE %in% c(
        "Glaziers"
      ) ~ "Glaziers",
      
      OCC_TITLE %in% c(
        "Insulation Workers, Floor, Ceiling, and Wall",
        "Insulation Workers, Mechanical"
      ) ~ "Insulation workers",
      
      OCC_TITLE %in% c(
        "Paving, Surfacing, and Tamping Equipment Operators"
      ) ~ "Paving, surfacing, and tamping equipment operators",
      
      OCC_TITLE %in% c(
        "Roofers"
      ) ~ "Roofers and slaters",
      
      OCC_TITLE %in% c(
        "Structural Iron and Steel Workers",
        "Reinforcing Iron and Rebar Workers"
      ) ~ "Structural metal workers",
      
      OCC_TITLE %in% c(
        "Earth Drillers, Except Oil and Gas"
      ) ~ "Drillers of earth",
      
      OCC_TITLE %in% c(
        "Helpers, Electricians",
        "Helpers, Painters, Paperhangers, Plasterers, and Stucco Masons",
        "Helpers, Roofers",
        "Helpers, Pipelayers, Plumbers, Pipefitters, and Steamfitters",
        "Helpers, Production Workers",
        "Construction and Building Inspectors"
      ) ~ "Misc. construction and related occupations",
      
      OCC_TITLE %in% c(
        "Rotary Drill Operators, Oil and Gas",
        "Service Unit Operators, Oil and Gas"
      ) ~ "Drillers of oil wells",
      
      OCC_TITLE %in% c(
        "Explosives Workers, Ordnance Handling Experts, and Blasters"
      ) ~ "Explosives workers",
      
      OCC_TITLE %in% c(
        "Underground Mining Machine Operators",
        "Continuous Mining Machine Operators",
        "Mine Cutting and Channeling Machine Operators"
      ) ~ "Miners",
      
      OCC_TITLE %in% c(
        "Rock Splitters, Quarry",
        "Roof Bolters, Mining",
        "Roustabouts, Oil and Gas",
        "Extraction Workers, All Other"
      ) ~ "Other mining occupations",
      
      # -----------------------------
      # PRODUCTION / PLANT
      # -----------------------------
      OCC_TITLE %in% c(
        "First-Line Supervisors of Production and Operating Workers"
      ) ~ "Production supervisors or foremen",
      
      OCC_TITLE %in% c(
        "Tool and Die Makers"
      ) ~ "Tool and die makers and die setters",
      
      OCC_TITLE %in% c(
        "Machinists"
      ) ~ "Machinists",
      
      OCC_TITLE %in% c(
        "Boilermakers"
      ) ~ "Boilermakers",
      
      OCC_TITLE %in% c(
        "Patternmakers, Metal and Plastic",
        "Patternmakers, Wood"
      ) ~ "Patternmakers and model makers",
      
      OCC_TITLE %in% c(
        "Engravers and Etchers"
      ) ~ "Engravers",
      
      OCC_TITLE %in% c(
        "Cabinetmakers and Bench Carpenters"
      ) ~ "Cabinetmakers and bench carpeters",
      
      OCC_TITLE %in% c(
        "Furniture Finishers"
      ) ~ "Furniture/wood finishers, other prec. wood workers",
      
      OCC_TITLE %in% c(
        "Tailors, Dressmakers, and Custom Sewers"
      ) ~ "Dressmakers, seamstresses, and tailors",
      
      OCC_TITLE %in% c(
        "Upholsterers"
      ) ~ "Upholsterers",
      
      OCC_TITLE %in% c(
        "Molders, Shapers, and Casters, Except Metal and Plastic"
      ) ~ "Hand molders and shapers, except jewelers",
      
      OCC_TITLE %in% c(
        "Bookbinders and Bookkeeping Workers"
      ) ~ "Bookbinders",
      
      OCC_TITLE %in% c(
        "Butchers and Meat Cutters"
      ) ~ "Butchers and meat cutters",
      
      OCC_TITLE %in% c(
        "Bakers"
      ) ~ "Bakers",
      
      OCC_TITLE %in% c(
        "Food Batchmakers"
      ) ~ "Batch food makers",
      
      OCC_TITLE %in% c(
        "Water and Wastewater Treatment Plant and System Operators"
      ) ~ "Water and sewage treatment plant operators",
      
      OCC_TITLE %in% c(
        "Power Plant Operators"
      ) ~ "Power plant operators",
      
      OCC_TITLE %in% c(
        "Stationary Engineers and Boiler Operators"
      ) ~ "Plant and system operators, stationary engineers",
      
      OCC_TITLE %in% c(
        "Chemical Plant and System Operators",
        "Gas Plant Operators",
        "Petroleum Pump System Operators, Refinery Operators, and Gaugers",
        "Plant and System Operators, All Other"
      ) ~ "Other plant and system operators",
      
      OCC_TITLE %in% c(
        "Lathe and Turning Machine Tool Setters, Operators, and Tenders, Metal and Plastic",
        "Milling and Planing Machine Setters, Operators, and Tenders, Metal and Plastic"
      ) ~ "Lathe, milling, and turning machine operatives",
      
      OCC_TITLE %in% c(
        "Rolling Machine Setters, Operators, and Tenders, Metal and Plastic"
      ) ~ "Rollers, roll hands, and finishers of metal",
      
      OCC_TITLE %in% c(
        "Drilling and Boring Machine Tool Setters, Operators, and Tenders, Metal and Plastic"
      ) ~ "Drilling and boring machine operators",
      
      OCC_TITLE %in% c(
        "Grinding, Lapping, Polishing, and Buffing Machine Tool Setters, Operators, and Tenders, Metal and Plastic"
      ) ~ "Grinding, abrading, buffing, and polishing workers",
      
      OCC_TITLE %in% c(
        "Foundry Mold and Coremakers",
        "Molding, Coremaking, and Casting Machine Setters, Operators, and Tenders, Metal and Plastic"
      ) ~ "Molders and casting machine operators",
      
      OCC_TITLE %in% c(
        "Heat Treating Equipment Setters, Operators, and Tenders, Metal and Plastic"
      ) ~ "Heat treating equipment operators",
      
      OCC_TITLE %in% c(
        "Sawing Machine Setters, Operators, and Tenders, Wood"
      ) ~ "Sawing machine operators and sawyers",
      
      OCC_TITLE %in% c(
        "Nail, Tacking, and Labeling Machine Setters, Operators, and Tenders"
      ) ~ "Nail, tacking, shaping and joining mach ops (wood)",
      
      OCC_TITLE %in% c(
        "Textile Winding, Twisting, and Drawing Out Machine Setters, Operators, and Tenders"
      ) ~ "Winding and twisting textile and apparel operatives",
      
      OCC_TITLE %in% c(
        "Textile Knitting and Weaving Machine Setters, Operators, and Tenders",
        "Textile Winding, Twisting, and Drawing Out Machine Setters, Operators, and Tenders"
      ) ~ "Knitters, loopers, and toppers textile operatives",
      
      OCC_TITLE %in% c(
        "Textile Bleaching and Dyeing Machine Operators and Tenders",
        "Textile Cutting Machine Setters, Operators, and Tenders"
      ) ~ "Textile cutting and dyeing machine operators",
      
      OCC_TITLE %in% c(
        "Sewing Machine Operators"
      ) ~ "Textile sewing machine operators",
      
      OCC_TITLE %in% c(
        "Shoe and Leather Workers and Repairers"
      ) ~ "Shoemaking machine operators",
      
      OCC_TITLE %in% c(
        "Pressers, Textile, Garment, and Related Materials"
      ) ~ "Clothing pressing machine operators",
      
      OCC_TITLE %in% c(
        "Packaging and Filling Machine Operators and Tenders"
      ) ~ "Packers, fillers, and wrappers",
      
      OCC_TITLE %in% c(
        "Extruding and Forming Machine Setters, Operators, and Tenders, Synthetic and Glass Fibers"
      ) ~ "Extruding and forming machine operators",
      
      OCC_TITLE %in% c(
        "Mixing and Blending Machine Setters, Operators, and Tenders"
      ) ~ "Mixing and blending machine operators",
      
      OCC_TITLE %in% c(
        "Separating, Filtering, Clarifying, Precipitating, and Still Machine Setters, Operators, and Tenders"
      ) ~ "Separating, filtering, and clarifying machine operators",
      
      OCC_TITLE %in% c(
        "Roasting, Baking, and Drying Machine Operators and Tenders"
      ) ~ "Food roasting and baking machine operators",
      
      OCC_TITLE %in% c(
        "Cleaning, Washing, and Metal Pickling Equipment Operators and Tenders"
      ) ~ "Washing, cleaning, and pickling machine operators",
      
      OCC_TITLE %in% c(
        "Paper Goods Machine Setters, Operators, and Tenders"
      ) ~ "Paper folding machine operators",
      
      OCC_TITLE %in% c(
        "Furnace, Kiln, Oven, Drier, and Kettle Operators and Tenders"
      ) ~ "Furnace, kiln, and oven operators, apart from food",
      
      OCC_TITLE %in% c(
        "Crushing, Grinding, Polishing, Mixing, and Blending Workers"
      ) ~ "Slicing, cutting, crushing and grinding machine",
      
      OCC_TITLE %in% c(
        "Photographic Process Workers and Processing Machine Operators"
      ) ~ "Photographic process workers",
      
      OCC_TITLE %in% c(
        "Welders, Cutters, Solderers, and Brazers"
      ) ~ "Welders, solderers, and metal cutters",
      
      OCC_TITLE %in% c(
        "Electrical, Electronics, and Electromechanical Assemblers, Except Coil Winders, Tapers, and Finishers"
      ) ~ "Assemblers of electrical equipment",
      
      OCC_TITLE %in% c(
        "Painting, Coating, and Decorating Workers"
      ) ~ "Painting and decoration occupations",
      
      # -----------------------------
      # TRANSPORT / LABOR
      # -----------------------------
      OCC_TITLE %in% c(
        "First-Line Supervisors of Transportation and Material Moving Workers"
      ) ~ "Supervisors of motor vehicle transportation",
      
      OCC_TITLE %in% c(
        "Heavy and Tractor-Trailer Truck Drivers",
        "Light Truck Drivers"
      ) ~ "Truck, delivery, and tractor drivers",
      
      OCC_TITLE %in% c(
        "Bus Drivers, School",
        "Bus Drivers, Transit and Intercity"
      ) ~ "Bus drivers",
      
      OCC_TITLE %in% c(
        "Taxi Drivers"
      ) ~ "Taxi cab drivers and chauffeurs",
      
      OCC_TITLE %in% c(
        "Parking Attendants"
      ) ~ "Parking lot attendants",
      
      OCC_TITLE %in% c(
        "Rail Transportation Workers, All Other",
        "Rail Yard Engineers, Dinkey Operators, and Hostlers",
        "Railroad Conductors and Yardmasters"
      ) ~ "Railroad conductors and yardmasters",
      
      OCC_TITLE %in% c(
        "Locomotive Engineers"
      ) ~ "Locomotive operators: engineers and firemen",
      
      OCC_TITLE %in% c(
        "Railroad Brake, Signal, and Switch Operators and Locomotive Firers"
      ) ~ "Railroad brake, coupler, and switch operators",
      
      OCC_TITLE %in% c(
        "Sailors and Marine Oilers",
        "Ship Engineers"
      ) ~ "Ship crews and marine engineers",
      
      OCC_TITLE %in% c(
        "Operating Engineers and Other Construction Equipment Operators"
      ) ~ "Operating engineers of construction equipment",
      
      OCC_TITLE %in% c(
        "Crane and Tower Operators",
        "Derrick Operators, Oil and Gas",
        "Hoist and Winch Operators",
        "Conveyor Operators and Tenders"
      ) ~ "Crane, derrick, winch, hoist, longshore operators",
      
      OCC_TITLE %in% c(
        "Excavating and Loading Machine and Dragline Operators",
        "Loading and Moving Machine Operators, Underground Mining"
      ) ~ "Excavating and loading machine operators",
      
      OCC_TITLE %in% c(
        "Helpers--Construction Trades"
      ) ~ "Helpers, constructions",
      
      OCC_TITLE %in% c(
        "Construction Laborers"
      ) ~ "Construction laborers",
      
      OCC_TITLE %in% c(
        "Helpers--Production Workers"
      ) ~ "Production helpers",
      
      OCC_TITLE %in% c(
        "Refuse and Recyclable Material Collectors"
      ) ~ "Garbage and recyclable material collectors",
      
      OCC_TITLE %in% c(
        "Machine Feeders and Offbearers"
      ) ~ "Machine feeders and offbearers",
      
      OCC_TITLE %in% c(
        "Gas Compressor and Gas Pumping Station Operators",
        "Service Station Attendants"
      ) ~ "Garage and service station related occupations",
      
      OCC_TITLE %in% c(
        "Vehicle and Equipment Cleaners"
      ) ~ "Vehicle washers and equipment cleaners",
      
      OCC_TITLE %in% c(
        "Packers and Packagers, Hand"
      ) ~ "Packers and packagers by hand",
      
      OCC_TITLE %in% c(
        "Laborers and Freight, Stock, and Material Movers, Hand"
      ) ~ "Laborers, freight, stock, and material handlers, n.e.c.",
      
      TRUE ~ NA_character_
    )
  )

# --------------------------------------------------
# 6. CHECK WHAT IS STILL UNMATCHED
# --------------------------------------------------

national %>%
  filter(is.na(OCC_TITLE_NEW)) %>%
  distinct(OCC_TITLE) %>%
  arrange(OCC_TITLE)

# --------------------------------------------------
# 7. AGGREGATE TOT_EMP BY AUTOMATION OCCUPATION
# --------------------------------------------------

national_totals <- national %>%
  filter(!is.na(OCC_TITLE_NEW)) %>%
  group_by(OCC_TITLE_NEW) %>%
  summarise(TOT_EMP = sum(TOT_EMP, na.rm = TRUE), .groups = "drop")

# --------------------------------------------------
# 8. MERGE TOT_EMP ONTO PROBABILITY DATA
# --------------------------------------------------

prob_auto_final <- prob_auto %>%
  left_join(national_totals, by = c("occupation" = "OCC_TITLE_NEW"))

View(prob_auto_final)


prob_auto_final %>% 
  select(-number) %>% 
  write.csv("final_prob_auto_w_weights.csv", row.names=FALSE)

# --------------------------------------------------
# 9. SEE WHICH PROBABILITY OCCUPATIONS STILL HAVE NO TOT_EMP
# --------------------------------------------------

prob_auto_final %>%
  filter(is.na(TOT_EMP)) %>%
  distinct(occupation) %>%
  arrange(occupation)

#still some that aren't coming over right but not manyl.....
