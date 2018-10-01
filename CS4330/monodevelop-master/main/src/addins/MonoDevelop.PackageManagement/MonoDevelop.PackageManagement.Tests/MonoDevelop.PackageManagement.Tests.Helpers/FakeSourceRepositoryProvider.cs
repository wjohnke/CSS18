﻿//
// FakeSourceRepositoryProvider.cs
//
// Author:
//       Matt Ward <matt.ward@xamarin.com>
//
// Copyright (c) 2016 Xamarin Inc. (http://xamarin.com)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

using System.Collections.Generic;
using NuGet.Configuration;
using NuGet.Protocol;
using NuGet.Protocol.Core.Types;

namespace MonoDevelop.PackageManagement.Tests.Helpers
{
	class FakeSourceRepositoryProvider : ISourceRepositoryProvider
	{
		public List<SourceRepository> Repositories = new List<SourceRepository> ();
		public FakePackageSourceProvider FakePackageSourceProvider = new FakePackageSourceProvider ();

		public IPackageSourceProvider PackageSourceProvider {
			get { return FakePackageSourceProvider; }
		}

		public SourceRepository CreateRepository (PackageSource source)
		{
			return CreateRepository (source, FeedType.Undefined);
		}

		public SourceRepository CreateRepository (PackageSource source, FeedType type)
		{
			return new SourceRepository (source, new INuGetResourceProvider[0]);
		}

		public IEnumerable<SourceRepository> GetRepositories ()
		{
			return Repositories;
		}

		public void AddRepositories (IEnumerable<PackageSource> sources)
		{
			foreach (PackageSource source in sources) {
				AddRepository (source);
			}
		}

		public void AddRepository (PackageSource source)
		{
			Repositories.Add (CreateRepository (source));
		}
	}
}

