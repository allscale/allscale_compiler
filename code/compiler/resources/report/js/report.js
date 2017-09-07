"use strict";

function determineLevelForEntry(entry) {
	var level = 'success';
	for (var i = 0; i < entry.issues.length; i++) {
		var issue = entry.issues[i];
		if (issue.severity == 'Warning') {
			level = 'warning';
		} else if (issue.severity == 'Error') {
			level = 'danger';
			break;
		}
	}
	return level;
}

function determineLevelForIssue(issue) {
	if (issue.severity == 'Error') {
		return 'danger';
	} else if (issue.severity == 'Warning') {
		return 'warning';
	} else {
		return 'info';
	}
}

function getHelpMessage(error_code) {
	if (error_code in report.help_messages) {
		return report.help_messages[error_code];
	} else {
		return ""
	}
}

function setProgress() {
	var successes = 0;
	var warnings = 0;
	var errors = 0;
	var count = Object.keys(report.conversions).length;

	for (var addr in report.conversions) {
		var entry = report.conversions[addr];
		var level = determineLevelForEntry(entry);

		if (level == 'danger') {
			errors++;
		} else if (level == 'warning') {
			warnings++;
		} else {
			successes++;
		}
	}

	$('#progress-num').text(`${successes} / ${count}`);

	$('#progress .progress-bar-success').attr('style', `width: ${100 * successes / count}%`);
	$('#progress .progress-bar-warning').attr('style', `width: ${100 * warnings  / count}%`);
	$('#progress .progress-bar-danger').attr ('style', `width: ${100 * errors    / count}%`);
}

function createSource(id, addr, location, source) {

	var $src = $('<pre>');
	var $head = $('<div>').addClass('location');
	var $link = $('<a>').attr('href', `#${id}`).attr('data-toggle', 'collapse');
	var $body = $('<code>').attr('id', id);

	if (location) {
		$src.attr('data-start', parseInt(location.substr(location.indexOf('@') + 1)));
		$head.append(
			$('<div>').addClass('nodeaddress internal').text(addr),
			$link.text(location)
		);
		$body.addClass('line-numbers');
	} else {
		$src.addClass('internal');
		$head.append(
			$link.addClass('nodeaddress').text(addr)
		);
	}

	if (source) {
		$body.addClass('language-cpp').text(source);
	} else {
		$body.addClass('language-txt').text('No location information');
	}

	return $src.append($head.append($link), $body);
}

function createBacktrace(addr, issue_index, issue, trace, index) {
	return createSource(
		`entry-${addr}-issue-${issue_index}-backtrace-${index}`,
		addr,
		trace.location,
		trace.source
	);
}

function createTags(issue) {
	if (!issue.tags) {
		return;
	}
	return $.map(issue.tags, function(tag) {
		return $('<div>').addClass('label label-default').text(tag);
	});
}

function createIssue(addr, issue, index) {
	return $('<div>')
		.addClass(`entry-issue panel panel-${determineLevelForIssue(issue)}`)
		.addClass(
			$.map(issue.tags, function(tag) {
				return `tag-${tag}`;
			}).join(' ')
		)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<div>')
						.addClass('entry-issue-error-code')
						.text(issue.error_code),
					$('<div>')
						.addClass('entry-issue-message')
						.append(
							$('<a>')
								.attr('href', `#entry-${addr}-issue-${index}`)
								.attr('data-toggle', 'collapse')
								.addClass('panel-title')
								.html(`<strong>${issue.severity}:</strong> ${issue.message}`)
						),
					$('<div>')
						.addClass('entry-issue-tags')
						.append(createTags(issue))
				),
			$('<div>')
				.attr('id', `entry-${addr}-issue-${index}`)
				.addClass('entry-issue-body collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$('<pre>')
								.addClass('entry-issue-detail')
								.text(issue.detail ? issue.detail : ''),
							$('<div>')
								.addClass('entry-issue-help')
								.text(getHelpMessage(issue.error_code)),
							createSource(`entry-${addr}-issue-${index}-source`, addr, issue.loc.location, issue.loc.source),
							$('<div>')
								.attr('id', `entry-${addr}-issue-${index}-backtrace`)
								.addClass('panel-group')
								.addClass('backtrace')
								.append(
									$('<div>').addClass('backtrace-text').text('Backtrace'),
									$.map(issue.backtrace, $.proxy(createBacktrace, null, addr, index, issue))
								)
						)
				)
		);
}

function createConversion(entry, addr) {
	var loc = entry.loc_user ? entry.loc_user : entry.loc;

	return $('<div>')
		.addClass(`entry panel panel-${determineLevelForEntry(entry)}`)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<i>').addClass('glyphicon glyphicon-file'),
					' ',
					$('<a>')
						.attr('href', `#entry-${addr}`)
						.attr('data-toggle', 'collapse')
						.addClass('panel-title')
						.text(loc.location)
				),
			$('<div>')
				.attr('id', `entry-${addr}`)
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$.map(entry.issues, $.proxy(createIssue, null, addr)),
							createSource(`entry-${addr}-source`, addr, loc.location, loc.source)
						)
				)
		);
}

function createRaw() {
	return $('<div>')
		.addClass('panel panel-default internal')
		.append(
			$('<div>')
				.addClass('panel-heading')
				.attr('style', 'text-align: center')
				.append(
					$('<a>')
						.attr('href', '#raw')
						.attr('data-toggle', 'collapse')
						.text('Raw Conversion Report')
				),
			$('<div>')
				.attr('id', 'raw')
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$('<pre>').text(JSON.stringify(report, null, 2))
						)
				)
		);
}

function setupControls() {
	// filter by tag
	{
		// collect all tags
		var tags = new Set();
		for (var addr in report.conversions) {
			var conversion = report.conversions[addr];
			for (var index = 0; index < conversion.issues.length; index++) {
				var issue = conversion.issues[index];
				if (issue.tags) {
					tags = new Set([...tags, ...issue.tags]);
				}
			}
		}
		tags = Array.from(tags);

		if (tags.length > 0) {
			$('#filters').append(
				$('<h4>').text('Filter Tags'),
				$.map(tags, function(tag) {
					return $('<div>')
						.addClass('checkbox')
						.append($('<label>').append(
							$('<input>')
								.attr('type', 'checkbox')
								.attr('checked', true)
								.change(function() {
									if (this.checked) {
										$(`.tag-${tag}`).show();
									} else {
										$(`.tag-${tag}`).hide();
									}
								}),
							tag
						))
				})
			);
		}

	}

	// internal
	{
		var $internals = $('.internal');

		$internals.hide();

		$('#internal-button').click(function() {
			$internals.toggle();
		});
	}
}

function main() {
	$('#main').append($.map(report.conversions, createConversion));

	// open if only 1
	if (Object.keys(report.conversions).length == 1) {
		for (var addr in report.conversions) {
			$(`#entry-${addr}`).collapse('show');
		}
	}

	// add raw block
	$('#main').append(createRaw());

	setProgress();
	setupControls();

	// fix initial collapse state of prism code blocks
	$('code[class*=language-]').addClass('collapse').attr('style', 'height: 0px');
}

main();
