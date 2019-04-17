using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using VMS.TPS.Common.Model.API;
using VMS.TPS.Common.Model.Types;
using System.Reflection;
using System.IO;
using System.Diagnostics;

namespace esapi_j
{
    class Program
    {
        private static StringBuilder stdErr = new StringBuilder("");
        public const string OUTDIR = @".\";
        private static Application app;

        [STAThread]
        static void Main(string[] args)
        {
            Console.WriteLine("Hello ESAPI");
            app = Application.CreateApplication(null, null);
            Execute();
            //try
            //{
            //    using (app = Application.CreateApplication(null, null)) //CreateApplication())
            //    {
            //        Execute();
            //    }
            //}
            //catch (Exception e)
            //{
            //    Console.Error.WriteLine(e.ToString());
            //}
            //Console.ReadKey(); 
            //System.Threading.Thread.Sleep(1000);
        }


        private static void stdErrHandler(object sendingProcess,
            DataReceivedEventArgs outLine)
        {
            if (!String.IsNullOrEmpty(outLine.Data))
            {
                stdErr.Append(Environment.NewLine + outLine.Data);
            }
        }

        static Boolean IsDiagnosisProstate(PlanSetup plan)
        {
            Boolean isProstate = false;
            Course course = plan.Course;
            foreach (var diagnosis in course.Diagnoses)
            {
                if (diagnosis.ClinicalDescription.ToLower().Contains("prostate"))
                {
                    isProstate = true;
                }
            }


            return isProstate;
        }

        static Boolean IsDiagnosisProstate(Course course)
        {
            Boolean isProstate = false;
            foreach (var diagnosis in course.Diagnoses)
            {
                if (diagnosis.ClinicalDescription.ToLower().Contains("prostate"))
                {
                    isProstate = true;
                }
            }

            return isProstate;
        }

        static Boolean IsPlanProstate(PlanSetup plan)
        {
            Boolean isProstate = false;
            if (plan.StructureSet != null && plan.StructureSet.Structures != null)
            {
                int score = 0;
                int rectum_cnt = 0;
                int fem_head_cnt = 0;
                int bladder_cnt = 0;
                foreach (var structure in plan.StructureSet.Structures)
                {
                    string structurename = structure.Id.ToString().ToLower();
                    if (structurename.Contains("rectum")) rectum_cnt = 1;
                    if (structurename.Contains("fem") && structurename.Contains("head")) fem_head_cnt = 1;
                    if (structurename.Contains("bladder")) bladder_cnt = 1;
                    //if (structure.Id.ToString().ToLower().Contains("tv")) score += 1;



                }
                score = rectum_cnt + fem_head_cnt + bladder_cnt;
                if (score == 3)
                {
                    isProstate = true;
                }
                else
                {
                    isProstate = false;
                }
                //Console.WriteLine($"{score}");
            }

            return isProstate;
        }

        static void DumpDVH(string filename, DVHData dvh)
        {
            System.IO.StreamWriter dvhFile = new System.IO.StreamWriter(filename);
            // write a header
            dvhFile.WriteLine("Dose,Volume");
            // write all dvh points for the PTV.
            foreach (DVHPoint pt in dvh.CurveData)
            {
                string line = string.Format("{0},{1}", pt.DoseValue.Dose, pt.Volume);
                dvhFile.WriteLine(line);
            }
            dvhFile.Close();
        }

        static Patient openPatient(string patientid)
        {
            Patient tmpPatient = null;
            for (int i = 0; i < 3; i++)
            {
                tmpPatient = app.OpenPatientById(patientid.Substring(0, patientid.Length - i));
                if (tmpPatient == null)
                {
                    continue;
                }
                else
                {
                    break;
                }
            }

            return tmpPatient;
        }

        public static int ComputeStringDistance(string s, string t)
        {
            int n = s.Length;
            int m = t.Length;
            int[,] d = new int[n + 1, m + 1];

            // Step 1
            if (n == 0)
            {
                return m;
            }

            if (m == 0)
            {
                return n;
            }

            // Step 2
            for (int i = 0; i <= n; d[i, 0] = i++)
            {
            }

            for (int j = 0; j <= m; d[0, j] = j++)
            {
            }

            // Step 3
            for (int i = 1; i <= n; i++)
            {
                //Step 4
                for (int j = 1; j <= m; j++)
                {
                    // Step 5
                    int cost = (t[j - 1] == s[i - 1]) ? 0 : 1;

                    // Step 6
                    d[i, j] = Math.Min(
                        Math.Min(d[i - 1, j] + 1, d[i, j - 1] + 1),
                        d[i - 1, j - 1] + cost);
                }
            }
            // Step 7
            return d[n, m];
        }

        static List<string> findStructure(PlanSetup plan, string strucName)
        {
            //bool heartFound = false;
            int sdist = 999;
            string foundStructureName = "";
            List<string> foundStructureNames = new List<string>();
            if (plan.StructureSet != null)
            {
                foreach (var structure in plan.StructureSet.Structures)
                {
                    if (structure.Id.ToString().ToLower().Contains(strucName.ToLower()))
                    {
                        //Console.Write(structure.Id);
                        //sw.WriteLine("\tHeart found:{0}, \tvolume = {1}, \t hasSegment:{2}", structure.Id, structure.Volume, structure.HasSegment);
                        if (structure.Volume>100 && structure.Volume<10000 && structure.HasSegment)
                        {
                            int theDistance = ComputeStringDistance(structure.Id.ToString().ToLower(), strucName.ToLower());
                            Console.WriteLine("{0} and {1} distance:{2}", structure.Id.ToString().ToLower(), strucName.ToLower(), theDistance);
                            foundStructureNames.Add(structure.Id.ToString());
                            if (theDistance < sdist)
                            {
                                foundStructureName = structure.Id.ToString();
                            }
                            sdist = theDistance;
                        }
                    }
                }
            }
            //foundStructureNames.Add(foundStructureName);
            if (foundStructureName == "")
            {
                return foundStructureNames;
            } else {
                var returnlist = new List<string>();
                returnlist.Add(foundStructureName);
                return returnlist;
            }
        }

        static void printDict(IDictionary<PlanSetupApprovalStatus, int> dict)
        {
            Console.WriteLine("\tStatus:{0}, \tCount = {1}", PlanSetupApprovalStatus.TreatmentApproved, dict[PlanSetupApprovalStatus.TreatmentApproved]);
            Console.WriteLine("\tStatus:{0}, \tCount = {1}", PlanSetupApprovalStatus.PlanningApproved, dict[PlanSetupApprovalStatus.PlanningApproved]);
        }

        static string getWholeInfoFromPlan(PlanSetup plan)
        {
            string infoStr = "";
            int i = 1;
            Console.Write("process plan:" + plan.Course.Id + ":" +  plan.Id+":");
            // 1
            Console.Write($" {i++}");
            infoStr += plan.UID.ToString();
            infoStr += "|";

            // 2
            Console.Write($" {i++}");
            infoStr += plan.Course.Patient.Id.ToString();
            infoStr += "|";

            // 3
            Console.Write($" {i++}");
            infoStr += plan.Course.Patient.LastName.ToString();
            infoStr += "|";

            // 4
            Console.Write($" {i++}");
            infoStr += plan.Course.Patient.FirstName.ToString();
            infoStr += "|";

            // 5
            Console.Write($" {i++}");
            infoStr += plan.Course.Patient.Sex.ToString();
            infoStr += "|";

            // 6
            Console.Write($" {i++}");
            infoStr += plan.Course.Patient.DateOfBirth.ToString();
            infoStr += "|";

            // 7
            Console.Write($" {i++}");
            infoStr += plan.Course.Id.ToString();
            infoStr += "|";

            // 8
            Console.Write($" {i++}");
            int diagnosisN = plan.Course.Diagnoses.Count();
            infoStr += diagnosisN.ToString();
            infoStr += "|";


            // 9
            Console.Write($" {i++}");
            if (diagnosisN>0)
            {
                infoStr += plan.Course.Diagnoses.ToArray()[0].ClinicalDescription.ToString();
            }
            infoStr += "|";

            // 10
            Console.Write($" {i++}");
            if (diagnosisN > 1)
            {
                infoStr += plan.Course.Diagnoses.ToArray()[1].ClinicalDescription.ToString();
            }
            infoStr += "|";

            // 11
            Console.Write($" {i++}");
            infoStr += diagnosisN.ToString();
            infoStr += "|";

            // 12
            Console.Write($" {i++}");
            if (diagnosisN > 0)
            {
                infoStr += plan.Course.Diagnoses.ToArray()[0].ClinicalDescription.ToString();
            }
            infoStr += "|";

            // 13
            Console.Write($" {i++}");
            if (diagnosisN > 1)
            {
                infoStr += plan.Course.Diagnoses.ToArray()[1].ClinicalDescription.ToString();
            }
            infoStr += "|";

            // 14
            Console.Write($" {i++}");
            if (diagnosisN > 2)
            {
                infoStr += plan.Course.Diagnoses.ToArray()[2].ClinicalDescription.ToString();
            }
            infoStr += "|";

            // 15
            Console.Write($" {i++}");
            if (plan.Course.CompletedDateTime.HasValue)
            {
                infoStr += "Completed";
            } else
            {
                infoStr += "Active";
            }
            infoStr += "|";

            // 16
            Console.Write($" {i++}");
            infoStr += plan.Course.StartDateTime.ToString();
            infoStr += "|";

            // 17
            Console.Write($" {i++}");
            if (plan.Course.CompletedDateTime.HasValue)
            {
                infoStr += plan.Course.CompletedDateTime.ToString();
            }
            else
            {
                infoStr += "";
            }
            infoStr += "|";

            // 18
            Console.Write($" {i++}");
            infoStr += plan.Id.ToString();
            infoStr += "|";

            // 19
            Console.Write($" {i++}");
            infoStr += plan.Name.ToString();
            infoStr += "|";

            // 20
            Console.Write($" {i++}");
            infoStr += plan.PlanIntent.ToString();
            infoStr += "|";

            // 21
            Console.Write($" {i++}");
            infoStr += plan.ApprovalStatus.ToString();
            infoStr += "|";

            // 22
            Console.Write($" {i++}");

            infoStr += plan.StructureSet.Id.ToString();
            infoStr += "|";

            // 23
            Console.Write($" {i++}");
            if (plan.TreatmentOrientation == null)
            {
                infoStr += "Unknown";
            } else
            {
                infoStr += plan.TreatmentOrientation.ToString();
            }
            
            infoStr += "|";

            // 24
            Console.Write($" {i++}");
            var foundTargets = plan.StructureSet.Structures.Where((x, index) => x.Id == plan.TargetVolumeID).ToArray();
            if(foundTargets.Count()>0)
            {
                infoStr += foundTargets[0].Volume.ToString();
                
            } else
            {
                infoStr += "TargetNoFound";
            }
            
            infoStr += "|";

            // 25
            Console.Write($" {i++}");
            infoStr += plan.DosePerFraction.ToString();
            infoStr += "|";

            // 26
            Console.Write($" {i++}");
            infoStr += plan.NumberOfFractions.ToString();
            infoStr += "|";

            // 27
            Console.Write($" {i++}");
            infoStr += plan.PlanNormalizationMethod.ToString();
            infoStr += "|";

            // 28
            Console.Write($" {i++}");
            if (plan.GetType() == typeof(IonPlanSetup))
            {
                infoStr += plan.ProtonCalculationModel.ToString();
            } else
            {
                infoStr += plan.PhotonCalculationModel.ToString();
            }
            
            infoStr += "|";

            if (plan.Dose == null)
            {
                // 29
                Console.Write($" {i++}");
                infoStr += "|";

                // 30
                Console.Write($" {i++}");
                infoStr += "|";

                // 31
                Console.Write($" {i++}");
                infoStr += "|";
            } else
            {
                // 29
                Console.Write($" {i++}");
                infoStr += plan.Dose.XRes.ToString();
                infoStr += "|";

                // 30
                Console.Write($" {i++}");
                infoStr += plan.Dose.YRes.ToString();
                infoStr += "|";

                // 31
                Console.Write($" {i++}");
                infoStr += plan.Dose.ZRes.ToString();
                infoStr += "|";
            }


            // 32
            Console.Write($" {i++}");
            var targets = plan.StructureSet.Structures.Where((x, index) => x.DicomType.ToString().Count() == 3 && x.DicomType.ToString().Contains("TV")).ToArray();
            var targetsCount = targets.Count();
            infoStr += targetsCount.ToString();
            infoStr += "|";

            // 33
            Console.Write($" {i++}");
            if (targetsCount>0)
            {
                infoStr += targets[0].Id.ToString();
            }
            infoStr += "|";

            // 34
            Console.Write($" {i++}");
            if (targetsCount > 0)
            {
                infoStr += targets[0].Volume.ToString();
            }
            infoStr += "|";

            // 35
            Console.Write($" {i++}");
            if (targetsCount > 1)
            {
                infoStr += targets[1].Id.ToString();
            }
            infoStr += "|";

            // 36
            Console.Write($" {i++}");
            if (targetsCount > 1)
            {
                infoStr += targets[1].Volume.ToString();
            }
            infoStr += "|";

            // 37
            Console.Write($" {i++}");
            if (targetsCount > 2)
            {
                infoStr += targets[2].Id.ToString();
            }
            infoStr += "|";

            // 38
            Console.Write($" {i++}");
            if (targetsCount > 2)
            {
                infoStr += targets[2].Volume.ToString();
            }
            infoStr += "|";

            // 39
            Console.Write($" {i++}");
            if (targetsCount > 3)
            {
                infoStr += targets[3].Id.ToString();
            }
            infoStr += "|";

            // 40
            Console.Write($" {i++}");
            if (targetsCount > 3)
            {
                infoStr += targets[3].Volume.ToString();
            }
            infoStr += "|";

            // 41
            Console.Write($" {i++}");
            if (targetsCount > 4)
            {
                infoStr += targets[4].Id.ToString();
            }
            infoStr += "|";

            // 42
            Console.Write($" {i++}");
            if (targetsCount > 4)
            {
                infoStr += targets[4].Volume.ToString();
            }
            infoStr += "|";

            // 43
            Console.Write($" {i++}");
            var hearts = plan.StructureSet.Structures.Where((x, index) => x.Id.ToString().ToLower().Contains("heart")).ToArray();
            var heartCount = hearts.Count();
            infoStr += heartCount.ToString();
            infoStr += "|";

            // 44
            Console.Write($" {i++}");
            if (heartCount>0)
            {
                infoStr += hearts[0].Id.ToString();
            }
            infoStr += "|";

            // 45
            Console.Write($" {i++}");
            if (heartCount > 0)
            {
                infoStr += hearts[0].Volume.ToString();
            }
            infoStr += "|";

            // 46
            Console.Write($" {i++}");
            if (heartCount > 1)
            {
                infoStr += hearts[1].Id.ToString();
            }
            infoStr += "|";

            // 47
            Console.Write($" {i++}");
            if (heartCount > 1)
            {
                infoStr += hearts[1].Volume.ToString();
            }
            infoStr += "|";

            // 48
            Console.Write($" {i++}");
            if (heartCount > 2)
            {
                infoStr += hearts[2].Id.ToString();
            }
            infoStr += "|";

            // 49
            Console.Write($" {i++}");
            if (heartCount > 2)
            {
                infoStr += hearts[2].Volume.ToString();
            }
            infoStr += "|";

            // 50
            Console.Write($" {i++}");
            if (heartCount > 3)
            {
                infoStr += hearts[3].Id.ToString();
            }
            infoStr += "|";

            // 51
            Console.Write($" {i++}");
            if (heartCount > 3)
            {
                infoStr += hearts[3].Volume.ToString();
            }
            infoStr += "|";

            // 52
            Console.Write($" {i++}");
            if (heartCount > 4)
            {
                infoStr += hearts[4].Id.ToString();
            }
            infoStr += "|";

            // 53
            Console.Write($" {i++}");
            if (heartCount > 4)
            {
                infoStr += hearts[4].Volume.ToString();
            }
            infoStr += "|";

            // 54
            Console.Write($" {i++}");
            var beams = plan.Beams.ToArray();
            infoStr += beams[0].Technique.ToString();
            infoStr += "|";

            // 55
            Console.Write($" {i++}");
            infoStr += beams.Count().ToString();
            infoStr += "|";

            bool e1 = false;
            bool e2 = false;
            bool e3 = false;
            bool e4 = false;
            bool e5 = false;

            foreach(var beam in beams)
            {
                if(beam.EnergyModeDisplayName.Contains("6X")) {
                    if (beam.EnergyModeDisplayName.Contains("FFF")) {
                        e2 = true;
                    } else
                    {
                        e1 = true;
                    }
                } else if (beam.EnergyModeDisplayName.Contains("10")) {
                    if (beam.EnergyModeDisplayName.Contains("FFF")) {
                        e4 = true;
                    }
                    else
                    {
                        e3 = true;
                    }
                } else if (beam.EnergyModeDisplayName.Contains("18")) {
                    e5 = true;
                }
            }

            // 56
            Console.Write($" {i++}");
            infoStr += e1.ToString();
            infoStr += "|";

            // 57
            Console.Write($" {i++}");
            infoStr += e2.ToString();
            infoStr += "|";

            // 58
            Console.Write($" {i++}");
            infoStr += e3.ToString();
            infoStr += "|";

            // 59
            Console.Write($" {i++}");
            infoStr += e4.ToString();
            infoStr += "|";

            // 60
            Console.Write($" {i++}");
            infoStr += e5.ToString();
            //infoStr += "|";
            Console.WriteLine("");


            return infoStr;
        }

        static void Execute()
        {

            //Patient aPatient = openPatient("2554128");
            //Course aCourse = aPatient.Courses.Where((x, index) => x.Id == "IMPAC").ToArray()[0];

            StreamWriter sw = new StreamWriter(".\\log.csv", false, Encoding.ASCII);
            sw.AutoFlush = true;
            sw.WriteLine("ID|MRN|Last Name|First Name|Sex|DOB|Course Name|Count of Course Attached Diagnoses|Course Attached Diagnosis 1|Course Attached Diagnosis 2|Count of Course Available Diagnoses|Course Available Diagnosis 1|Course Available Diagnosis 2|Course Available Diagnosis 3|Course Status|Course Start Date|Course Completed Date|Plan ID|Plan Name|Plan Intent|Plan Approval Status|Plan Structure Set ID|Plan Patient Position|Plan Target Volume|Dose Per Fraction|Number of Fractions|Normalization Mode|Calculation Model|Dose Resolution Width|Dose Resolution Height|Plane Separation|Count of Target Structures|Plan Target Structure 1|Plan Target Volume 1|Plan Target Structure 2|Plan Target Volume 2|Plan Target Structure 3|Plan Target Volume 3|Plan Target Structure 4|Plan Target Volume 4|Plan Target Structure 5|Plan Target Volume 5|Count of Heart Structures|Plan Heart Structure 1|Plan Heart Volume 1|Plan Heart Structure 2|Plan Heart Volume 2|Plan Heart Structure 3|Plan Heart Volume 3|Plan Heart Structure 4|Plan Heart Volume 4|Plan Heart Structure 5|Plan Heart Volume 5|Plan Technique|Plan Number of Fields|Plan Field Energy 6X|Plan Field Energy 6FFF|Plan Field Energy 10X|Plan Field Energy 10FFF|Plan Field Energy 18X");

            List<string> lines = System.IO.File.ReadLines("patientList.txt").ToList();

            foreach (string patientID in lines)
            {
                Patient tmpPatient = openPatient(patientID);

                Console.WriteLine(patientID + ": ");
                if (tmpPatient == null)
                {
                    Console.WriteLine(" does not exist!!!!!!!!!!!!!!");
                    continue;
                }
                foreach (var course in tmpPatient.Courses)
                {
                    if(course.Id.ToString().ToLower().Contains("impac"))
                    {
                        Console.WriteLine("skip course: "+course.Id);
                        continue;
                    }
                    foreach (var plan in course.PlanSetups)
                    {

                        string info;
                        try
                        {
                            info = getWholeInfoFromPlan(plan);
                        }
                        catch (Exception e)
                        {
                            info = new String('|', 59);
                        }
                        sw.WriteLine(info);
                    }
                    foreach (var plan in course.IonPlanSetups)
                    {
                        string info;
                        try
                        {
                            info = getWholeInfoFromPlan(plan);
                        }
                        catch (Exception e)
                        {
                            info = new String('|', 59) ;
                        }
                        sw.WriteLine(info);
                    }
                }
            
                app.ClosePatient();
            }




            sw.Flush();
            sw.Close();
            //Console.WriteLine(app.PatientSummaries.ToArray().Length);
            Console.WriteLine("Bye bye ESAPI!");

        }

    }
}
