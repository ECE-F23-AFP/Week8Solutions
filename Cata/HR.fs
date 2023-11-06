module AlgebraicDataTypes.hr

// 6.9

// 1 
type Business =
    | SubDepartment of string * Business list
    
// 2
type DepartmentInfo = { name: string; grossIncome: decimal }
type Business' =
    | Department of DepartmentInfo * Business' list
    
// 3
let rec businessToDepartments business =
    let rec businessToDepartmentsTR list (Department (di, subDepartments)) =
        match subDepartments with
        | [] -> (di.name, di.grossIncome) :: list
        | ts -> List.fold (fun state elem -> businessToDepartmentsTR state elem) ((di.name, di.grossIncome)::list) ts
    businessToDepartmentsTR [] business
    
// 4
let rec totalIncome business =
    let rec totalIncomeTR total (Department (di, subDepartments)) =
        match subDepartments with
        | [] -> di.grossIncome + total
        | ts -> List.fold (fun state e -> totalIncomeTR state e) (di.grossIncome+total) ts
    totalIncomeTR 0.0M business
