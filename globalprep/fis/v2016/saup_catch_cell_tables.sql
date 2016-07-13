CREATE TABLE [dbo].[taxon](
	[TaxonKey] [int] NOT NULL,
	[Scientific_Name] [nvarchar](255) NOT NULL,
	[Common_Name] [nvarchar](255) NOT NULL,
	[FunctionalGroupDescription] [nvarchar](50) NOT NULL
) 

CREATE TABLE [dbo].[AllocationData](
	[UniversalDataID] [int] NOT NULL,
	[DataLayerID] [tinyint] NOT NULL,
	[FishingEntityID] [int] NOT NULL,
	[Year] [int] NOT NULL,
	[TaxonKey] [int] NOT NULL,
	[InputTypeID] [tinyint] NOT NULL,
	[sector_type_name] [nvarchar](25) NULL,
	[catch_type_name] [nvarchar](25) NULL,
	[reporting_status_name] [nvarchar](25) NULL
)

CREATE TABLE [dbo].[AllocationResult](
	[UniversalDataID] [int] NOT NULL,
	[CellID] [int] NOT NULL,
	[AllocatedCatch] [float] NULL
)

CREATE TABLE [dbo].[FishingEntity](
	[FishingEntityID] [smallint] NOT NULL,
	[Name] [nvarchar](100) NOT NULL
) ON [PRIMARY]