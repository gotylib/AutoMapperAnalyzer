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
            .ForMember(dest => dest.Name, opt => opt.MapFrom(src => src.Name));
    }
}

public class Examples
{
    public class UserDto { public string Name { get; set; } }
    public class UserEntity { public string Name { get; set; }}// Ошибка: int → string
    
    public void ToStars()
    {

        
    }
}