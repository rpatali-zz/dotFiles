﻿using System.Linq;
using System.Xml.Linq;
using NUnit.Framework;
using OmniSharp.ProjectManipulation.AddReference;
using Should;

namespace OmniSharp.Tests.ProjectManipulation.AddReference
{
    [TestFixture]
    public class AddFileReferenceTests : AddReferenceTestsBase
    {
        [Test]
        public void CanAddFileReference()
        {
            var project = CreateDefaultProjectWithFileReference();

            Solution.Projects.Add(project);

            var request = new AddReferenceRequest
            {
                Reference = @"c:\test\packages\SomeTest\lib\net40\Some.Test.dll",
                FileName = @"c:\test\one\test.cs"
            };

            var expectedXml = XDocument.Parse(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                    <ItemGroup>
                        <Reference Include=""Hello.World"">
                            <HintPath>..\packages\HelloWorld\lib\net40\Hello.World.dll</HintPath>
                        </Reference>
                        <Reference Include=""Some.Test"">
                            <HintPath>..\packages\SomeTest\lib\net40\Some.Test.dll</HintPath>
                        </Reference>
                    </ItemGroup>
                </Project>");

            var handler = new AddReferenceHandler(Solution, new AddReferenceProcessorFactory(Solution, new IReferenceProcessor[] { new AddFileReferenceProcessor() }, new FakeFileSystem()));
            handler.AddReference(request);

            project.AsXml().ToString().ShouldEqual(expectedXml.ToString());
            ((FakeAssembly)project.References.First(r => r.GetType() == typeof(FakeAssembly))).AssemblyPath.ShouldEqual(@"c:\test\packages\SomeTest\lib\net40\Some.Test.dll");
        }

        [Test]
        public void CanAddFileReferenceWhenNoReferencesExist()
        {
            var project = CreateDefaultProject();

            Solution.Projects.Add(project);

            var request = new AddReferenceRequest
            {
                Reference = @"c:\test\packages\SomeTest\lib\net40\Some.Test.dll",
                FileName = @"c:\test\one\test.cs"
            };

            var expectedXml = XDocument.Parse(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                    <ItemGroup>
                        <Reference Include=""Some.Test"">
                            <HintPath>..\packages\SomeTest\lib\net40\Some.Test.dll</HintPath>
                        </Reference>
                    </ItemGroup>
                </Project>");

            var handler = new AddReferenceHandler(Solution, new AddReferenceProcessorFactory(Solution, new IReferenceProcessor[] { new AddFileReferenceProcessor() }, new FakeFileSystem()));
            handler.AddReference(request);

            project.AsXml().ToString().ShouldEqual(expectedXml.ToString());
            ((FakeAssembly)project.References.First(r => r.GetType() == typeof(FakeAssembly))).AssemblyPath.ShouldEqual(@"c:\test\packages\SomeTest\lib\net40\Some.Test.dll");
        }

        [Test]
        public void ShouldNotAddDuplicateFileReference()
        {
            var project = CreateDefaultProjectWithFileReference();

            Solution.Projects.Add(project);

            var request = new AddReferenceRequest
            {
                Reference = @"c:\test\packages\HelloWorld\lib\net40\Hello.World.dll",
                FileName = @"c:\test\one\test.cs"
            };

            var expectedXml = XDocument.Parse(@"
                <Project xmlns=""http://schemas.microsoft.com/developer/msbuild/2003"">
                    <ItemGroup>
                        <Compile Include=""Test.cs""/>
                    </ItemGroup>
                    <ItemGroup>
                        <Reference Include=""Hello.World"">
                            <HintPath>..\packages\HelloWorld\lib\net40\Hello.World.dll</HintPath>
                        </Reference>
                    </ItemGroup>
                </Project>");

            var handler = new AddReferenceHandler(Solution, new AddReferenceProcessorFactory(Solution, new IReferenceProcessor[] { new AddFileReferenceProcessor() }, new FakeFileSystem()));
            handler.AddReference(request);

            project.AsXml().ToString().ShouldEqual(expectedXml.ToString());
            project.References.FirstOrDefault(r => r.GetType() == typeof(FakeAssembly)).ShouldBeNull();
        }
    }
}
