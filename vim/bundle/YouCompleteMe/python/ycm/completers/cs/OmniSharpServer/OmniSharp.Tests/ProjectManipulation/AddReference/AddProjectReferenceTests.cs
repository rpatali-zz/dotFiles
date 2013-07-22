﻿using System;
using System.Xml.Linq;
using NUnit.Framework;
using OmniSharp.Common;
using OmniSharp.ProjectManipulation.AddReference;
using OmniSharp.Solution;
using Should;

namespace OmniSharp.Tests.ProjectManipulation.AddReference
{
    [TestFixture]
    public class AddProjectReferenceTests : AddReferenceTestsBase
    {
        [Test]
        public void CanAddProjectReferenceWhenNoProjectReferencesExist()
        {
            var projectOne = CreateDefaultProject();

            var projectTwoId = Guid.NewGuid();
            var projectTwo = new FakeProject("faketwo", @"c:\test\two\fake2.csproj", projectTwoId);
            projectTwo.Title = "Project Two";
            projectTwo.AddFile("some content", @"c:\test\two\test.cs");
            projectTwo.XmlRepresentation = XDocument.Parse(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                </Project>");

            var expectedXml = XDocument.Parse(string.Format(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                    <ItemGroup>
                        <ProjectReference Include=""..\one\fake1.csproj"">
                            <Project>{0}</Project>
                            <Name>Project One</Name>
                        </ProjectReference>
                    </ItemGroup>
                </Project>", string.Concat("{", projectOne.ProjectId.ToString().ToUpperInvariant(), "}")));

            Solution.Projects.Add(projectOne);
            Solution.Projects.Add(projectTwo);

            var request = new AddReferenceRequest
                {
                    Reference = @"fake1",
                    FileName = @"c:\test\two\test.cs"
                };

            var handler = new AddReferenceHandler(Solution, new AddReferenceProcessorFactory(Solution, new IReferenceProcessor[] { new AddProjectReferenceProcessor(Solution) }, new FileSystem()));
            handler.AddReference(request);

            projectTwo.AsXml().ToString().ShouldEqual(expectedXml.ToString());
        }

        [Test]
        public void CanAddProjectReferenceWhenProjectReferencesExist()
        {
            var projectOne = CreateDefaultProject();

            var projectTwoId = Guid.NewGuid();
            var projectTwo = new FakeProject("faketwo", @"c:\test\two\fake2.csproj", projectTwoId);
            projectTwo.Title = "Project Two";
            projectTwo.AddFile("some content", @"c:\test\two\test.cs");
            projectTwo.XmlRepresentation = XDocument.Parse(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                    <ItemGroup>
                        <ProjectReference Include=""..\existing\project.csproj"">
                            <Project>{1-2-3-4}</Project>
                            <Name>Existing Project</Name>
                        </ProjectReference>
                    </ItemGroup>
                </Project>");

            var expectedXml = XDocument.Parse(string.Format(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                    <ItemGroup>
                        <ProjectReference Include=""..\existing\project.csproj"">
                            <Project>{{1-2-3-4}}</Project>
                            <Name>Existing Project</Name>
                        </ProjectReference>
                        <ProjectReference Include=""..\one\fake1.csproj"">
                            <Project>{0}</Project>
                            <Name>Project One</Name>
                        </ProjectReference>
                    </ItemGroup>
                </Project>", string.Concat("{", projectOne.ProjectId.ToString().ToUpperInvariant(), "}")));

            Solution.Projects.Add(projectOne);
            Solution.Projects.Add(projectTwo);

            var request = new AddReferenceRequest
                {
                    Reference = @"fake1",
                    FileName = @"c:\test\two\test.cs"
                };

            var handler = new AddReferenceHandler(Solution, new AddReferenceProcessorFactory(Solution, new IReferenceProcessor[] { new AddProjectReferenceProcessor(Solution) }, new FileSystem()));
            handler.AddReference(request);

            projectTwo.AsXml().ToString().ShouldEqual(expectedXml.ToString());
        }

        [Test]
        public void WillNotAddDuplicateProjectReference()
        {
            var projectOne = CreateDefaultProject();

            var projectTwoId = Guid.NewGuid();
            var projectTwo = new FakeProject("faketwo", @"c:\test\two\fake2.csproj", projectTwoId);
            projectTwo.Title = "Project Two";
            projectTwo.AddFile("some content", @"c:\test\two\test.cs");

            var xml = string.Format(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                    <ItemGroup>
                        <ProjectReference Include=""..\one\fake1.csproj"">
                            <Project>{0}</Project>
                            <Name>Project One</Name>
                        </ProjectReference>
                    </ItemGroup>
                </Project>", string.Concat("{", projectOne.ProjectId.ToString().ToUpperInvariant(), "}"));

            projectTwo.XmlRepresentation = XDocument.Parse(xml);

            var expectedXml = XDocument.Parse(xml);

            Solution.Projects.Add(projectOne);
            Solution.Projects.Add(projectTwo);

            var request = new AddReferenceRequest
                {
                    Reference = @"fake1",
                    FileName = @"c:\test\two\test.cs"
                };

            var handler = new AddReferenceHandler(Solution, new AddReferenceProcessorFactory(Solution, new IReferenceProcessor[] { new AddProjectReferenceProcessor(Solution) }, new FileSystem()));
            var response = handler.AddReference(request);

            projectTwo.AsXml().ToString().ShouldEqual(expectedXml.ToString());
            response.Message.ShouldEqual("Reference already added");
        }

        [Test]
        public void ShouldNotAddCircularReference()
        {
            var projectOne = CreateDefaultProject();

            var projectTwoId = Guid.NewGuid();
            var projectTwo = new FakeProject("faketwo", @"c:\test\two\fake2.csproj", projectTwoId);
            projectTwo.Title = "Project Two";
            projectTwo.AddFile("some content", @"c:\test\two\test.cs");
            projectTwo.AddReference(new ProjectReference(Solution, "Project One"));

            var xml = string.Format(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                    <ItemGroup>
                        <ProjectReference Include=""..\one\fake1.csproj"">
                            <Project>{0}</Project>
                            <Name>Project One</Name>
                        </ProjectReference>
                    </ItemGroup>
                </Project>", string.Concat("{", projectOne.ProjectId.ToString().ToUpperInvariant(), "}"));

            var expectedXml = XDocument.Parse(xml);
            projectTwo.XmlRepresentation = XDocument.Parse(xml);
            
            Solution.Projects.Add(projectOne);
            Solution.Projects.Add(projectTwo);

            var request = new AddReferenceRequest
            {
                Reference = @"fake2",
                FileName = @"c:\test\one\test.cs"
            };

            var handler = new AddReferenceHandler(Solution, new AddReferenceProcessorFactory(Solution, new IReferenceProcessor[] { new AddProjectReferenceProcessor(Solution) }, new FileSystem()));
            var response = handler.AddReference(request);

            projectTwo.AsXml().ToString().ShouldEqual(expectedXml.ToString());
            response.Message.ShouldEqual("Reference will create circular dependency");
        }
    }
}