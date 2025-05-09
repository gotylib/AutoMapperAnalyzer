// ReSharper disable UnusedType.Global
// ReSharper disable UnusedMember.Global

using AutoMapper;

namespace AutoMapperAnalyzer.Sample;

// If you don't see warnings, build the Analyzers Project.

public class a : Profile
{
    public a()
    {
        
        CreateMap<Examples.UserDto, Examples.UserEntity>()
            .ForMember(dest => dest.Namg, opt => opt.MapFrom(src => src.Name))
            .ReverseMap()
            .ForPath(desc => desc.Name, opt => opt.MapFrom(src => src.Namg));
        CreateMap<too.Examples.A, too.Examples.B>()
            .ReverseMap();
        CreateMap<Sample.Examples.C, Sample.Examples.D>()
            .ReverseMap();
    }
}

public class Examples
{
    public class UserDto
    {
        public string Name { get; set; }
        
        public too.Examples.B a { get; set; }
        
    }

    public class UserEntity
    {
        public string Namg { get; set; }
        
        public too.Examples.A a { get; set; }
    }

    public class C
    {
        public string FirstName { get; set; }
        
        public string LastName { get; set; }
    }
    
    public class D
    {
        public string FirstName { get; set; }
        
        public string LastName { get; set; }
    }

    public void ToStars()
    {


    }
}